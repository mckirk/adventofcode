import socket

def submit_answer(port: int, answer: str):
    try:
        with socket.create_connection(("localhost", port), timeout=0.1) as sock:
            sock.sendall(answer.encode("utf-8") + b"\n")
    except (ConnectionRefusedError, socket.timeout):
        print("Submission server not running; answer not submitted.")
