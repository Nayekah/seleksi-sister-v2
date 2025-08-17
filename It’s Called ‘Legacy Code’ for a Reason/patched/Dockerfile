FROM python:3.11-slim

RUN apt-get update && apt-get install -y --no-install-recommends \
    gnucobol \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /workspace

COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt

COPY app.py index.html main.cob accounts.txt ./

RUN cobc -x main.cob -o main

EXPOSE 8000

ENTRYPOINT ["sh", "-c", "\
if [ \"$1\" = \"--apply-interest\" ]; then \
    echo 'Starting banking with auto interest mode...'; \
    ./main --apply-interest & \
    exec uvicorn app:app --host 0.0.0.0 --port 8000; \
else \
    echo 'Starting normal banking mode...'; \
    exec uvicorn app:app --host 0.0.0.0 --port 8000; \
fi", "--"]