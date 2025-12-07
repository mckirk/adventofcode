#pragma once

#include <string>
#include <iostream>
#include <sys/socket.h>
#include <netinet/in.h>
#include <unistd.h>

void submit_answer(int port, const string& answer) {
    // open socket to localhost:port and send answer
    int sock = socket(AF_INET, SOCK_STREAM, 0);
    if (sock < 0) {
        cerr << "Error creating socket" << endl;
        return;
    }

    struct sockaddr_in server_addr;
    server_addr.sin_family = AF_INET;
    server_addr.sin_port = htons(port);
    server_addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);

    if (connect(sock, (struct sockaddr*)&server_addr, sizeof(server_addr)) < 0) {
        cerr << "Error connecting to server on port " << port << endl;
        close(sock);
        return;
    }

    string msg = answer + "\n";
    send(sock, msg.c_str(), msg.length(), 0);
    close(sock);
}