#' Serve phantasus.
#'
#' \code{servePhantasus} starts http server handling phantasus static
#' files and opencpu server.
#'
#' @param host Host to listen.
#'
#' @param port Single integer port to listen on.
#'
#' @param staticRoot Path to static files with phantasus.js
#'     (on local file system).
#'
#' @param preloadedDir Full path to directory with preloaded files.
#'
#' @param openInBrowser Boolean value which states if application will
#'     be automatically loaded in default browser.
#'
#' @param quiet Boolean value which states whether the connection log
#'     should be hidden (default: TRUE)
#'
#' @param background Boolean value which states whether the server
#'     should be started in background (default: FALSE). Returns an
#'     httpuv server handle when \code{TRUE}.
#'
#' @return In \code{background=TRUE} mode, the httpuv server handle.
#'     Otherwise does not return (blocks until interrupted).
#'
#' @import opencpu
#' @import httpuv
#' @importFrom utils getFromNamespace
#' @importFrom parallel makeCluster stopCluster
#' @export
#'
#' @examples
#' \dontrun{
#' # Returns handle immediately
#' s <- servePhantasus(background=TRUE)
#' s$stop()
#' }
#'
#' httpuv::stopAllServers() # can be used if handle is lost
servePhantasus <- function(host = getPhantasusConf("host"),
                           port = getPhantasusConf("port"),
                           staticRoot = getPhantasusConf("static_root"),
                           preloadedDir = getPhantasusConf("preloaded_dir"),
                           openInBrowser = TRUE,
                           quiet = TRUE,
                           background = FALSE) {

    if (!opencpu:::win_or_mac()) {
        if (! "unix" %in% utils::installed.packages()) {
            if (interactive() && menu(c("Yes", "No"),
                     title= paste("Couldn't find the required `unix` package, do you want to install it?")) == "1") {
                install.packages("unix")
            } else {
                stop("Phantasus can't work without `unix` package, please install it")
            }
        }
        run_worker <- NULL
    } else {
        #### this fragment is adopted from opencpu::ocpu_start_server function
        #### https://github.com/opencpu/opencpu/blob/master/R/start.R
        #### :ToDo: remove code duplication

        # set root home for workers
        Sys.setenv("OCPU_MASTER_HOME" = opencpu:::tmp_root())
        on.exit(Sys.unsetenv("OCPU_MASTER_HOME"))

        # import
        sendCall <- getFromNamespace('sendCall', 'parallel')
        recvResult <- getFromNamespace('recvResult', 'parallel')
        preload <- "opencpu"

        # worker pool
        pool <- list()

        # add new workers if needed
        add_workers <- function(n = 1){
            if(length(pool) < 2){
                cl <- parallel::makeCluster(n)
                lapply(cl, sendCall, fun = function(){
                    lapply(preload, getNamespace)
                    options(phantasusCacheDir = cacheDir,
                            phantasusPreloadedDir = preloadedDir)
                    Sys.getpid()
                }, args = list())
                pool <<- c(pool, cl)
            }
        }

        # get a worker
        get_worker <- function(){
            if(!length(pool))
                add_workers(1)
            node <- pool[[1]]
            pool <<- pool[-1]
            pid <- recvResult(node)
            if(inherits(pid, "try-error"))
                warning("Worker preload error: ", pid, call. = FALSE, immediate. = TRUE)
            node$pid <- pid
            structure(list(node), class = c("SOCKcluster", "cluster"))
        }

        # main interface
        run_worker <- function(fun, ..., timeout = NULL){
            res <- tryCatch({
                if(length(timeout)){
                    setTimeLimit(elapsed = timeout)
                    on.exit(setTimeLimit(cpu = Inf, elapsed = Inf), add = TRUE)
                }
                cl <- get_worker()
                on.exit(kill_workers(cl), add = TRUE)
                node <- cl[[1]]
                sendCall(node, fun, list(...))
                recvResult(node)
            }, error = function(e){
                if(grepl("elapsed time limit", e$message)){
                    tools::pskill(node$pid)
                    stop(sprintf("Timeout reached: %ds (see rlimit.post in user.conf)", timeout))
                }
                stop(e)
            })
            if(inherits(res, "try-error"))
                stop(res)
            res
        }

        kill_workers <- function(cl){
            parallel::stopCluster(cl) # does not work when child is busy
        }

        add_workers(2)
        on.exit(kill_workers(structure(pool, class = c("SOCKcluster", "cluster"))), add = TRUE)
    }

    app <- buildPhantasusApp(staticRoot = staticRoot,
                             preloadedDir = preloadedDir,
                             run_worker = run_worker)

    utils::capture.output(type = "output", {
        tryCatch({
            server <- startServer(host, port, app = app)
            message(sprintf(
                "Server was started with following parameters: host=%s, port=%s",
                host,
                port))
        },
        error = function(e) {
            stop(paste(e,
                       "The reason may be that requested port", port,
                       "is occupied with some other application"))
        })

        if (openInBrowser) {
            url <- sprintf("http://%s:%s", host, port)
            utils::browseURL(url)
            message(paste(url, "have been opened in your default browser.\n",
                          "If nothing happened, check your 'browser'",
                          "option with getOption('browser')",
                          "or open the address manually."))
        }

        if (background) {
            return(server)
        }

        on.exit(stopServer(server))
        service(0)
    }, split=!quiet)
}

buildPhantasusApp <- function(staticRoot = getPhantasusConf("static_root"),
                               preloadedDir = getPhantasusConf("preloaded_dir"),
                               run_worker = NULL) {
    if (nchar(staticRoot) == 0) {
        staticRoot <- system.file("www/phantasus.js", package = "phantasus")
    }
    cacheDir <- normalizePath(getPhantasusConf("cache_root"))
    preloadedDir <- if (is.null(preloadedDir)) {
        NULL
    } else {
        normalizePath(preloadedDir)
    }

    if (!dir.exists(cacheDir) ||
            !areCacheFoldersValid(getPhantasusConf("cache_folders"))) {
        stopPhantasus()
    }
    options(phantasusCacheDir = cacheDir,
            phantasusPreloadedDir = preloadedDir)

    selfCheck()

    releaseJSfile <- file.path(tempdir(), "RELEASE.js")
    generateReleaseJS(releaseJSfile)

    ocpu_handler <- opencpu:::rookhandler("/phantasus/ocpu",
                                          worker_cb = run_worker)

    app <- list(
        call = function(req) {
            path <- req$PATH_INFO
            if (startsWith(path, "/phantasus/ocpu")) {
                ocpu_handler(req)
            } else if (path == "/phantasus/RELEASE.js") {
                content <- readBin(releaseJSfile, "raw",
                                   n = file.info(releaseJSfile)$size)
                list(status  = 200L,
                     headers = list("Content-Type" = "application/javascript"),
                     body    = content)
            } else if (path == "/" || path == "") {
                list(status  = 301L,
                     headers = list(Location = "/phantasus/index.html"),
                     body    = "")
            } else {
                list(status = 404L, headers = list(), body = "Not found\n")
            }
        },
        staticPaths = list(
            "/phantasus/ocpu"      = httpuv::excludeStaticPath(),
            "/phantasus/geo"       = httpuv::staticPath(
                                         getPhantasusConf("cache_folders")$geo_path,
                                         fallthrough = FALSE,
                                         indexhtml   = FALSE),
            "/phantasus/preloaded" = httpuv::staticPath(
                                         cacheDir,
                                         fallthrough = FALSE,
                                         indexhtml   = FALSE),
            "/phantasus"           = httpuv::staticPath(
                                         staticRoot,
                                         fallthrough = TRUE,
                                         indexhtml   = TRUE)
        )
    )
    app
}
