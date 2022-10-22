package com.example.rodrigobange684006endassignment.service;

import com.example.rodrigobange684006endassignment.database.Database;
import com.example.rodrigobange684006endassignment.model.ResultMessage;
import com.example.rodrigobange684006endassignment.model.User;
import javafx.collections.FXCollections;

import java.util.ArrayList;
import java.util.List;

public class UserService {
    // Database
    Database database;

    // Variables
    ArrayList<User> users;

    // Constructor
    public UserService(Database database) {
        this.database = database;
        users = new ArrayList<>(database.getUsers());
    }

    /**
     * Checks if the username and password combination is valid.
     * @param username Username to validate.
     * @param password Password to validate.
     * @return Returns the result as a Boolean and String message.
     */
    public ResultMessage validateLogin(String username, String password) {
        // Check if user exists and then check if password is correct
        for (User user : users) {
            if (user.getUsername().equals(username) && user.getPassword().equals(password)) {
                // Instead of message return the name to display
                String shortname = user.getFirstname().substring(0,1) + ". " + user.getLastname();
                return new ResultMessage(true, shortname);
            }
        }
        // Display generic message
        return new ResultMessage(false, "Incorrect credentials. Invalid username or password.");
    }
}
