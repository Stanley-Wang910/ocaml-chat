import socket
import select
import sys
import os


def clear_line():
    sys.stdout.write("\033[F")
    sys.stdout.write("\033[K")


def format_message(message, is_own=False):
    terminal_width = os.get_terminal_size().columns
    if is_own:
        # Right align own messages with padding
        return message.rjust(terminal_width)
    else:
        # Left align others' messages
        return message


server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

if len(sys.argv) != 3:
    print("Usage: Script, IP Address, Port Number")
    exit()


# takes first arg as IP
IP_ADDRESS = str(sys.argv[1])

# takes second arg as port number
PORT = int(sys.argv[2])

server.connect((IP_ADDRESS, PORT))


while True:

    # possible input streams list
    sockets_list = [sys.stdin, server]

    """
    input situation 1:
        user wants to give manual input to send to other users
    input situation 2: 
        server is sending a message to be printed ON screen

    so, if serv wants to send ->
        if condition = true
    if user wants to send -> 
        if condition = false
    """

    read_sockets, write_socket, error_socket = select.select(sockets_list, [], [])

    for sock in read_sockets:
        if sock == server:
            message = sock.recv(2048)
            print(format_message(message.decode()))
        else:
            message = sys.stdin.readline()
            if message.strip():
                server.send(message.encode())
                clear_line()
                formatted_msg = format_message(f"You: {message.strip()}", True)
                print(formatted_msg)
                sys.stdout.flush()


server.close()
