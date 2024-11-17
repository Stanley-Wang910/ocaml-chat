import socket
import select
import sys
from _thread import *


"""
first arg AF_INET <- address domain of socket, used when we have an Internet 
Domain with any two hosts

second arg SOCK_STREAM <- is the type type of socket, in this case
representing a stream of data / charaters that are read continously
"""

server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

server.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)

# check correct arg length

if len(sys.argv) != 3:
    print("Usage: Script, IP Address, Port Number")
    exit()

# takes first arg as IP
IP_ADDRESS = str(sys.argv[1])


# takes second arg as port number
PORT = int(sys.argv[2])


# bind serv to IP at PORT
# client needs to be aware of these params

server.bind((IP_ADDRESS, PORT))


# listens to up to 100 active conns, can be inc +

server.listen(100)

clients = {}


def client_thread(conn, addr):

    try:
        username = conn.recv(2048).decode().strip()  # .strip() removes newlines
        clients[conn] = username
        conn.send(str.encode("welcome"))
        broadcast(f"\n{username} joined", conn)

        while True:
            try:
                message = conn.recv(2048)  # assuming byte length up to 2048?
                if message:
                    # print message + addr of user who sent message on server

                    decoded = message.decode().strip()
                    print("<" + addr[0] + "> " + decoded)

                    # call broadcast function to send message to all
                    send_message = f"<{username}> {decoded}"
                    broadcast(send_message, conn)

                else:
                    # may have no content if connection is broken, in this remove
                    remove(conn)
                    break

            except:
                continue
    except:
        remove(conn)


# broadcast the message to all clients who are not the one sending the message


def broadcast(msg, conn):
    disconnected = []
    for client in clients:
        if client != conn:  # if client not sender
            try:
                client.send(msg.encode())
            except:
                disconnected.append(client)

    for client in disconnected:
        remove(client)


# remove object from list created at beginning
def remove(client):
    if client in clients:
        username = clients[client]
        broadcast(f"{username} left the chat", client)  # Notify before removing
        del clients[client]
        try:
            client.close()
        except:
            pass


while True:

    """
    accept connection req, and stores:
        1. connnection <- socket object for that user
        2. IP address of the recently conned client
    """
    conn, addr = server.accept()

    print(addr[0] + " connected")

    # start thread for every user that connects
    start_new_thread(client_thread, (conn, addr))

conn.close()
server.close()
