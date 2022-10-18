package com.example.rodrigobange684006endassignment.model;

public class Item {
    int itemCode;
    Boolean available;
    String title;
    String author;

    public Item(int itemCode, Boolean available, String title, String author){
        this.itemCode = itemCode;
        this.available = available;
        this.title = title;
        this.author = author;
    }
}
