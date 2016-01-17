import socket
import json

HOST = ''   
PORT = 2024

s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.connect((HOST, PORT))

while True:
    s.sendall('GET lala.conf\n')

    data = s.recv(1024)
    
    print data

s.close()

