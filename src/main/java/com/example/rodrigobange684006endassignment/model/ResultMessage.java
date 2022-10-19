package com.example.rodrigobange684006endassignment.model;

public class ResultMessage {
    // Variables
    Boolean result;
    String message;

    // Properties
    public Boolean getResult() {
        return result;
    }

    public void setResult(Boolean result) {
        this.result = result;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    // Constructor
    public ResultMessage(Boolean result, String message) {
        this.result = result;
        this.message = message;
    }
}
