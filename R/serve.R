#' Serve phantasus.
#'
#' \code{servePhantasus} starts http server handling phantasus static
#' files and opencpu server.
#'
#' @param host Host to listen.
#'
#' @param port Port or integer vector of ports to listen on. A vector
#'     of ports is unix-only and starts one server instance per port.
#'
#' @param staticRoot Path to static files with phantasus.js
#'     (on local file system).
#'
#' @param preloadedDir Full path to directory with preloaded files.
#'
#' @param openInBrowser Boolean value which states if application will
#'     be automatically loaded in default browser. Not supported when
#'     multiple ports are specified.
#'
#' @param quiet Boolean value which states whether the connection log
#'     should be hidden (default: TRUE)
#'
#' @param background Boolean value which states whether the server
#'     should be started in background (default: FALSE). Only supported
#'     for single-port mode; returns an httpuv server handle.
#'
#' @return In \code{background=TRUE} mode, the httpuv server handle.
#'     In multi-port mode, does not return (blocks until all child
#'     instances exit). In single-port blocking mode, does not return.
#'
#' @import opencpu
#' @import httpuv
#' @import Rook
#' @importFrom utils getFromNamespace
#' @importFrom parallel makeCluster stopCluster
#' @export
#'
#' @examples
#' \dontrun{
#' # Single-port, returns handle
#' s <- servePhantasus(background=TRUE)
#' s$stop()
#'
#' # Multi-port blocking (unix only); send SIGINT to the process to stop
#' servePhantasus(port=8001:8004)
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

    if (opencpu:::win_or_mac() && length(port) > 1) {
        stop("Multiport configuration is unix-only")
    }
    if (length(port) > 1 && background) {
        stop("background=TRUE is not supported for multiple ports")
    }
    if (nchar(staticRoot) == 0){
        staticRoot = system.file("www/phantasus.js", package = "phantasus")
    }
    cacheDir <- normalizePath(getPhantasusConf("cache_root"))
    preloadedDir <- if (is.null(preloadedDir)){
        NULL
    } else{
        normalizePath(preloadedDir)
    }

    if (!dir.exists(cacheDir) | !areCacheFoldersValid(getPhantasusConf("cache_folders")) ){
        stopPhantasus()
    }
    options(phantasusCacheDir = cacheDir,
            phantasusPreloadedDir = preloadedDir)

    selfCheck()
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


    releaseJSfile <- file.path(tempdir(), "RELEASE.js")
    generateReleaseJS(releaseJSfile)

    subPathStatic <- function (targetDirectory, subPath) {
        static <- Rook::File$new(targetDirectory)

        function (env) {
            env$PATH_INFO <- unlist(strsplit(env$PATH_INFO, subPath, fixed=TRUE))[2]
            static$call(env)
        }
    }

    app <- Rook::URLMap$new(`/phantasus/ocpu` = opencpu:::rookhandler("/phantasus/ocpu", worker_cb=run_worker),
                            `/phantasus/geo` = subPathStatic(getPhantasusConf("cache_folders")$geo_path, '/phantasus/geo'),
                            `/phantasus/preloaded` = subPathStatic(cacheDir, '/phantasus/'),
                            `/phantasus/RELEASE.js` = subPathStatic(tempdir(), '/phantasus/'),
                            `/phantasus/?` = subPathStatic(staticRoot, '/phantasus/'),
                            `/?` = Rook::Redirect("/phantasus/index.html"))

    if (length(port) == 1) {
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
    } else {
        # unix only, blocking
        if (openInBrowser) {
            stop("Can't open in browser with multiple instances")
        }
        gc() # clean up unneeded stuff before forking
        parallel::mclapply(port,  function(p) {
            utils::capture.output(type = "output", {
                tryCatch({
                    server <- startServer(host, p, app = app)
                    message(sprintf(
                        "Server was started with following parameters: host=%s, port=%s",
                        host,
                        p))
                    on.exit(stopServer(server))
                    service(0)
                },
                error = function(e) {
                    stop(paste(e,
                               "The reason may be that requested port", p,
                               "is occupied with some other application"))
                })
            }, split=!quiet)
        }, mc.cores=length(port))
    }
}
