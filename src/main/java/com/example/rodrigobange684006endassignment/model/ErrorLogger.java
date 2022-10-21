package com.example.rodrigobange684006endassignment.model;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.PrintWriter;
import java.time.LocalDateTime;

public class ErrorLogger {
    String logFile = "ErrorLog.txt";

    public void log(Exception ex) {
        try {
            // If file exists, append, else write to new file
            FileWriter fileWriter = new FileWriter(logFile, true);
            BufferedWriter bufferedWriter = new BufferedWriter(fileWriter);
            PrintWriter printWriter = new PrintWriter(bufferedWriter, true);
            printWriter.println("Start_________________");
            printWriter.println("Error occurred at " + LocalDateTime.now());
            printWriter.println("Message: " + ex.getMessage());
            ex.printStackTrace(printWriter);
            printWriter.println("End_________________");
            printWriter.println("");
        }
        catch (Exception ie) {
            throw new RuntimeException("An issue occurred trying to log the Exception.", ie);
        }
    }
}
