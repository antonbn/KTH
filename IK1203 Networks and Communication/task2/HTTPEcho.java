import java.net.*;
import java.io.*;

public class HTTPEcho {
    public static void main(String[] args) throws Exception {
        String clientSentence;
        int port = Integer.parseInt(args[0]);

        final ServerSocket welcomeSocket = new ServerSocket(port);
        final String header = "HTTP/1.1 200 OK" + "\r\n" + "\r\n";

        do {
            final Socket connectionSocket = welcomeSocket.accept();
            final BufferedReader inFromClient = new BufferedReader(new
                    InputStreamReader(connectionSocket.getInputStream()));
            final DataOutputStream outToClient = new DataOutputStream(connectionSocket.getOutputStream());

            final StringBuilder output = new StringBuilder();
            while (!(clientSentence = inFromClient.readLine()).isEmpty()) {
                output.append(clientSentence);
                output.append("\r\n");
            }

            outToClient.writeBytes(header);
            outToClient.writeBytes(output.toString());

            inFromClient.close();
            outToClient.close();
            connectionSocket.close();
        } while (!clientSentence.isEmpty());
    }
}

