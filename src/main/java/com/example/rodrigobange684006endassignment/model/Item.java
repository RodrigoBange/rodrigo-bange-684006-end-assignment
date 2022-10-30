package com.example.rodrigobange684006endassignment.model;

import java.io.Serializable;
import java.time.LocalDateTime;
public class Item implements Serializable {
    // Values
    int itemCode;
    Boolean available = true;
    String title;
    String author;
    int lenderCode;
    LocalDateTime lendingDate;

    // Properties
    public int getItemCode() {
        return itemCode;
    }

    public Boolean getAvailable() {
        return available;
    }

    public void setAvailable(Boolean available) {
        this.available = available;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle (String title){
        this.title = title;
    }

    public String getAuthor() {
        return author;
    }

    public void setAuthor(String author) {
        this.author = author;
    }
    public int getLenderCode() {
        return lenderCode;
    }

    public void setLenderCode(int lenderCode) {
        this.lenderCode = lenderCode;
    }

    public LocalDateTime getLendingDate() {
        return lendingDate;
    }

    public void setLendingDate(LocalDateTime lendingDate) {
        this.lendingDate = lendingDate;
    }

    // Constructor
    public Item(int itemCode, String title, String author) {
        this.itemCode = itemCode;
        this.title = title;
        this.author = author;
    }
}
