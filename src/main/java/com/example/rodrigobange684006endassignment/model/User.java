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
    String firstname;
    public String getFirstname() {
        return firstname;
    }
    String lastname;
    public String getLastname() {
        return lastname;
    }

    public User (String username, String password, String firstname, String lastname){
        this.username = username;
        this.password = password;
        this.firstname = firstname;
        this.lastname = lastname;
    }
}
