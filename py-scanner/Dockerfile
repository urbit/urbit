FROM python:3.9

RUN apt-get update
RUN apt-get install -y libleveldb-dev
WORKDIR /code
COPY src/requirements.txt .
RUN pip install -r requirements.txt

ADD src .
RUN python setup.py install