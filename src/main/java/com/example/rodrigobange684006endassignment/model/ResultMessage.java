package com.example.rodrigobange684006endassignment.model;

public class ResultMessage {
    // Variables
    Boolean result;
    String message;

    // Properties
    public Boolean getResult() {
        return result;
    }

    public String getMessage() {
        return message;
    }

    // Constructor
    public ResultMessage(Boolean result, String message) {
        this.result = result;
        this.message = message;
    }
}
