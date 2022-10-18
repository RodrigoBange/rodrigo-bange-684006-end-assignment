package com.example.rodrigobange684006endassignment.model;

import java.util.Date;

public class Member {
    int identifier;
    String firstName;
    String lastName;
    Date dateOfBirth;

    public Member(int identifier, String firstName, String lastName, Date dateOfBirth){
        this.identifier = identifier;
        this.firstName = firstName;
        this.lastName = lastName;
        this.dateOfBirth = dateOfBirth;
    }
}
