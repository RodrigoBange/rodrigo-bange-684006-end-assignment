package com.example.rodrigobange684006endassignment.service;

import com.example.rodrigobange684006endassignment.database.Database;
import com.example.rodrigobange684006endassignment.model.ResultMessage;
import com.example.rodrigobange684006endassignment.model.User;

import java.util.List;

public class UserService {
    // Database
    Database database;

    List<User> users;

    public List<User> getUsers() {
        return users;
    }

    public UserService(Database database) {
        this.database = database;
        // users = database.getUsers();
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
            if (user.getUsername().equals(username)) {
                if (user.getPassword().equals(password)) {
                    return new ResultMessage(true, "Successfully logged in.");
                }
                else {
                    return new ResultMessage(false, "Password is invalid.");
                }
            }
        }
        return new ResultMessage(false, "User does not exist.");
    }
}
