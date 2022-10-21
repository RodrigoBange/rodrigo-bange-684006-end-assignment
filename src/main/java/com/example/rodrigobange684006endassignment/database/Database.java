package com.example.rodrigobange684006endassignment.database;

import com.example.rodrigobange684006endassignment.model.Item;
import com.example.rodrigobange684006endassignment.model.Member;
import com.example.rodrigobange684006endassignment.model.User;
import javafx.collections.ObservableList;

import java.util.List;

public class Database {

    public Database() {

    }

    public ObservableList<Item> GetItems() {
        return null;
    }
    public ObservableList<Member> GetMembers() {
        return null;
    }
    public List<User> GetUsers() {
        return null;
    }

    //TODO: Add methods to retrieve and save to file.
    abstract void add(T entity);
    abstract void remove(T entity);
    abstract void update(T entity);
}
