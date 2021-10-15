In order to run local algorithm in docker you need to have Docker installed 
([installation](https://docs.docker.com/get-docker/)). 
Once the docker is installed and running, in command line go to ```src/R``` folder and execute the following commands:

```
# First we build the image 
docker build -t rproject .

# Then we create a container, where we run the AlgorithmsTest.R

docker run --name algorithm rproject

```

In the terminal you will see the output of the R script. 

