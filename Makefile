IMAGE  ?= myproject
TAG    ?= latest

.PHONY: build shell run

## build: build the Docker image
build:
	docker build -t $(IMAGE):$(TAG) .

## shell: drop into an interactive bash shell inside the image
shell:
	docker run --rm -it -v $(PWD):/home/rproject -w /home/rproject $(IMAGE):$(TAG) /bin/bash

## run: run the pipeline inside the container (set CMD in Dockerfile or override here)
run:
	docker run --rm -v $(PWD):/home/rproject -w /home/rproject $(IMAGE):$(TAG) Rscript run.R
