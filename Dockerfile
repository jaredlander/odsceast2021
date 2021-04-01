FROM rstudio/plumber:next

LABEL maintainer="ODSC East 2021"

RUN Rscript -e "install.packages(c('here', 'workflows', 'recipes', 'parsnip', 'readr', 'jsonlite', 'xgboost'), repos='https://packagemanager.rstudio.com/all/__linux__/focal/latest', Ncpus=4)"

COPY code/plumber.r .
COPY code/mod0.rds .

EXPOSE 8000

CMD ["plumber.r"]
