FROM ubuntu:22.04

# Install system dependencies
RUN apt-get update && apt-get install -y \
    python3 \
    python3-pip \
    gnucobol \
    && apt-get clean

# Set working directory to /server and copy files
WORKDIR /server
COPY . .

# Install Python dependencies
RUN pip3 install -r requirements.txt

# Set Flask environment variables
ENV FLASK_APP=app
ENV FLASK_RUN_HOST=0.0.0.0
ENV FLASK_RUN_PORT=10000

# Run the Flask app
CMD ["flask", "run"]
