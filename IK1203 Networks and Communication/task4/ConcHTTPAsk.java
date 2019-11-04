import java.net.*;
import java.io.*;

public class ConcHTTPAsk {

    public static void main(String[] args) throws IOException {
        final int serverPort = Integer.parseInt(args[0]);
        final ServerSocket welcomeSocket = new ServerSocket(serverPort);

        while (true) {
            Runnable socket = new ConcSocket(welcomeSocket);
            new Thread(socket).run();
        }
    }
}

class ConcSocket implements Runnable {
    final Socket connectionSocket;

    public ConcSocket(final ServerSocket welcomeSocket) throws IOException {
        connectionSocket = welcomeSocket.accept();
    }

    @Override
    public void run() {
        try {
            final BufferedReader inFromClient = new BufferedReader(new
                    InputStreamReader(connectionSocket.getInputStream()));
            final DataOutputStream outToClient = new DataOutputStream(connectionSocket.getOutputStream());

            final String file = inFromClient.readLine().split(" ")[1];
            final String host = inFromClient.readLine().split(" ")[1];
            final URL url = new URL("http://" + host + file);

            if ("/ask".equals(url.getPath())) {
                final String[] pairs = url.getQuery().split("&");
                String hostname = "", data = null;
                int port = 0;

                for (String pair : pairs) {
                    String[] paramValue = pair.split("=");
                    switch (paramValue[0]) {
                        case "hostname":
                            hostname = paramValue[1];
                            break;
                        case "port":
                            port = Integer.parseInt(paramValue[1]);
                            break;
                        case "string":
                            data = paramValue[1];
                            break;
                    }
                }

                try {
                    final String response = TCPClient.askServer(hostname, port, data);

                    final String header = "HTTP/1.1 200 OK" + "\r\n" + "\r\n";
                    outToClient.writeBytes(header);
                    outToClient.writeBytes(response);
                } catch (IOException ex) {
                    final String header = "HTTP/1.1 400 Bad Request" + "\r\n" + "\r\n";
                    outToClient.writeBytes(header);
                }
            } else {
                final String header = "HTTP/1.1 404 Not Found" + "\r\n" + "\r\n";
                outToClient.writeBytes(header);
            }

            inFromClient.close();
            outToClient.close();
            connectionSocket.close();
        } catch (IOException ex) {

        }
    }
}

class TCPClient {

    public static String askServer(final String hostname, final int port, final String toServer) throws IOException {
        if (toServer == null) {
            return askServer(hostname, port);
        }

        final Socket clientSocket = new Socket(hostname, port);
        clientSocket.setSoTimeout(5000);

        final DataOutputStream outToServer = new DataOutputStream(clientSocket.getOutputStream());
        final BufferedReader inFromServer = new BufferedReader(new InputStreamReader(clientSocket.getInputStream()));

        outToServer.writeBytes(toServer + "\r\n");

        String line;
        final StringBuilder response = new StringBuilder(inFromServer.readLine());
        try {
            while ((line = inFromServer.readLine()) != null) {
                response.append("\r\n");
                response.append(line);
            }
        } catch (SocketTimeoutException ex) {
            // Nothing
        }

        clientSocket.close();
        return response.toString();
    }

    public static String askServer(String hostname, int port) throws IOException {
        final Socket clientSocket = new Socket(hostname, port);
        clientSocket.setSoTimeout(5000);

        final BufferedReader inFromServer = new BufferedReader(new InputStreamReader(clientSocket.getInputStream()));

        String line;
        final StringBuilder response = new StringBuilder(inFromServer.readLine());
        try {
            while ((line = inFromServer.readLine()) != null) {
                response.append("\r\n");
                response.append(line);
            }

            clientSocket.close();
        } catch (SocketTimeoutException ex) {
            clientSocket.close();
        }

        return response.toString();
    }
}
