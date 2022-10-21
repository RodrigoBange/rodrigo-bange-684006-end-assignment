package com.example.rodrigobange684006endassignment.model;

import java.time.LocalDate;

public class Member {
    // Variables
    int identifier;
    String firstName;
    String lastName;
    LocalDate dateOfBirth;

    // Properties
    public int getIdentifier() {
        return identifier;
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

    public LocalDate getDateOfBirth() {
        return dateOfBirth;
    }

    public void setDateOfBirth(LocalDate dateOfBirth) {
        this.dateOfBirth = dateOfBirth;
    }

    // Constructor
    public Member(int identifier, String firstName, String lastName, LocalDate dateOfBirth){
        this.identifier = identifier;
        this.firstName = firstName;
        this.lastName = lastName;
        this.dateOfBirth = dateOfBirth;
    }
}
