package tcpclient;

import java.net.*;
import java.io.*;

public class TCPClient {
    
    public static String askServer(final String hostname, final int port, final String toServer) throws  IOException {
        if (toServer == null) {
            return askServer(hostname, port);
        }

        final Socket clientSocket = new Socket(hostname, port);
        clientSocket.setSoTimeout(5000);

        final DataOutputStream outToServer = new DataOutputStream(clientSocket.getOutputStream());
        final BufferedReader inFromServer = new BufferedReader(new InputStreamReader(clientSocket.getInputStream()));

        outToServer.writeBytes(toServer + '\n');

        String line;
        final StringBuilder response = new StringBuilder(inFromServer.readLine());
        try {
            while ((line = inFromServer.readLine()) != null) {
                response.append("\n");
                response.append(line);
            }

            clientSocket.close();
        } catch (SocketTimeoutException ex) {
            clientSocket.close();
        }

        return response.toString();
    }

    public static String askServer(String hostname, int port) throws  IOException {
        final Socket clientSocket = new Socket(hostname, port);
        clientSocket.setSoTimeout(5000);


        final BufferedReader inFromServer = new BufferedReader(new InputStreamReader(clientSocket.getInputStream()));

        String line;
        final StringBuilder response = new StringBuilder(inFromServer.readLine());
        try {
            while ((line = inFromServer.readLine()) != null) {
                response.append("\n");
                response.append(line);
            }

            clientSocket.close();
        } catch (SocketTimeoutException ex) {
            clientSocket.close();
        }

        return response.toString();
    }
}

