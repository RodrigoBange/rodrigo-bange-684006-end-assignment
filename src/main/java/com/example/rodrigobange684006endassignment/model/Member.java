package com.example.rodrigobange684006endassignment.model;

import java.util.Date;

public class Member {
    // Variables
    int identifier;
    String firstName;
    String lastName;
    Date dateOfBirth;

    // Properties
    public int getIdentifier() {
        return identifier;
    }

    public void setIdentifier(int identifier) {
        this.identifier = identifier;
    }

    public String getFirstName() {
        return firstName;
    }

    public void setFirstName(String firstName) {
        this.firstName = firstName;
    }

    public String getLastName() {
        return lastName;
    }

    public void setLastName(String lastName) {
        this.lastName = lastName;
    }

    public Date getDateOfBirth() {
        return dateOfBirth;
    }

    public void setDateOfBirth(Date dateOfBirth) {
        this.dateOfBirth = dateOfBirth;
    }

    // Constructor
    public Member(int identifier, String firstName, String lastName, Date dateOfBirth){
        this.identifier = identifier;
        this.firstName = firstName;
        this.lastName = lastName;
        this.dateOfBirth = dateOfBirth;
    }
}
