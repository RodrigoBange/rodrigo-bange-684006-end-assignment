package com.example.rodrigobange684006endassignment.model;

import java.io.Serializable;

public class User implements Serializable {
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
