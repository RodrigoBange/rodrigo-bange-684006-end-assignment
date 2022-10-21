package com.example.rodrigobange684006endassignment.model;

public class User {
    String username;
    public String getUsername() {
        return username;
    }
    String password;
    public String getPassword() {
        return password;
    }

    public User (String username, String password){
        this.username = username;
        this.password = password;
    }
}
