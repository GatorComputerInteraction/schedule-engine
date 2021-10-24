FROM docker.io/swipl:8.4.0

WORKDIR /app

ADD . .
CMD ["swipl", "entry.pl"]
