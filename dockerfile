# start from the rocker/r-ver image
FROM rstudio/plumber

# install the linux libraries needed for plumber
RUN apt-get update -qq && apt-get install -y \
  libssl-dev \
  libcurl4-gnutls-dev \
  zlib1g-dev \
  libsodium-dev
  
# install the required R packages
RUN R -e "install.packages('readxl', repos='http://cran.rstudio.com/', version='1.4.2')"
RUN R -e "install.packages('dplyr', repos='http://cran.rstudio.com/', version='1.1.1')"
RUN R -e "install.packages('tidyr', repos='http://cran.rstudio.com/', version='1.3.0')"
RUN R -e "install.packages('purrr', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('formattable', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('scales', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('plotly', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('lubridate', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('quantmod', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('quadprog', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('rio', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('fredr', repos='http://cran.rstudio.com/')"


# copy everything from the current directory into the container
COPY / /

# open port 80 to traffic
EXPOSE 80

# when the container starts, start the main.R script
ENTRYPOINT ["Rscript", "runPlumberAPI.R"]