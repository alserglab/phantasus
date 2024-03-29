ARG PREIMAGE_NAME=alserglab/phantasus-preimage:latest
FROM $PREIMAGE_NAME
ARG TARGET_BRANCH=master
ARG PHANTASUS_BUILD
ARG GITHUB_PAT
ENV OCPU_MASTER_HOME=/var/phantasus/ocpu-root
ENV R_USER_CONFIG_DIR=/etc

# RUN apt install -y git && git clone -b ${TARGET_BRANCH} --recursive https://github.com/ctlab/phantasus /root/phantasus

RUN R -e 'BiocManager::install(c("rhdf5client", "phantasusLite"))'

COPY . /root/phantasus

RUN R -e 'devtools::install("/root/phantasus", dependencies=TRUE, upgrade=FALSE, build_vignettes=TRUE); remove.packages("BH")'

# install the most recent rhdf5client to support ARCHS4 v2.3 count files
RUN  apt-get -y update && \
apt-get -y install  git && \
git clone -b main --recursive https://github.com/vjcitn/rhdf5client.git /root/rhdf5client && \
git clone -b main --recursive https://github.com/ctlab/phantasusLite.git /root/phantasusLite
RUN R -e 'devtools::install("/root/rhdf5client", dependencies=TRUE, upgrade=FALSE); devtools::install("/root/phantasusLite", dependencies=TRUE, upgrade=FALSE);'

RUN printf "window.PHANTASUS_BUILD='$PHANTASUS_BUILD';" >> /root/phantasus/inst/www/phantasus.js/RELEASE.js

RUN R -e "phantasus:::generateReleaseJS('/root/phantasus/inst/www/phantasus.js/RELEASE.js', '$PHANTASUS_BUILD')"

RUN cp -r /root/phantasus/inst/www/phantasus.js /var/www/html/phantasus


RUN cp -r /root/phantasus/inst/configs/nginx  /etc/
RUN cp -r /root/phantasus/inst/configs/opencpu  /etc/
RUN cp -r /root/phantasus/inst/configs/apache2  /etc/
RUN cp /root/phantasus/inst/configs/index.html /var/www/html/
RUN cp -f /root/phantasus/inst/docker-entrypoint.sh /usr/bin/docker-entrypoint.sh


RUN a2dissite default-ssl.conf
RUN a2dissite 000-default.conf
RUN a2dissite rstudio.conf

EXPOSE 8000

RUN echo "LC_ALL=en_US.UTF-8" >> /etc/environment
RUN echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen
RUN echo "LANG=en_US.UTF-8" > /etc/locale.conf
RUN locale-gen en_US.UTF-8

ENV OCPU_USER=www-data


RUN mkdir -p ${R_USER_CONFIG_DIR}/R && cp -r /root/phantasus/inst/configs/R/phantasus ${R_USER_CONFIG_DIR}/R/ &&\
chown -R ${OCPU_USER} ${R_USER_CONFIG_DIR}/R/phantasus

RUN mkdir -p /var/phantasus/cache && chown ${OCPU_USER} /var/phantasus/cache
RUN mkdir -p /var/phantasus/preloaded && chown ${OCPU_USER} /var/phantasus/preloaded
RUN mkdir -p /var/phantasus/ocpu-root && chown -R ${OCPU_USER} /var/phantasus/ocpu-root
RUN mkdir -p /var/cache/nginx && chown -R ${OCPU_USER} /var/cache/nginx

RUN rm -rf /root/phantasus/inst

RUN rm /var/log/apache2/access.log /var/log/apache2/error.log /var/log/opencpu/apache_access.log /var/log/opencpu/apache_error.log

CMD /usr/bin/docker-entrypoint.sh

