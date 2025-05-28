# Use a base image that supports Python + GnuCOBOL
FROM ubuntu:22.04

# Install system dependencies
RUN apt-get update && apt-get install -y \
    python3 \
    python3-pip \
    gnucobol \
    && apt-get clean

# Copy project files
WORKDIR /app
COPY . .

# Install Python dependencies
RUN pip3 install -r requirements.txt

# Expose your Flask app
ENV FLASK_APP=server/app.py
ENV FLASK_RUN_HOST=0.0.0.0
ENV FLASK_RUN_PORT=10000

# Start Flask server
CMD ["flask", "run"]
