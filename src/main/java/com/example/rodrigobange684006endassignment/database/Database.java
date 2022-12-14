package com.example.rodrigobange684006endassignment.database;

import com.example.rodrigobange684006endassignment.model.ErrorLogger;
import com.example.rodrigobange684006endassignment.model.Item;
import com.example.rodrigobange684006endassignment.model.Member;
import com.example.rodrigobange684006endassignment.model.User;

import java.io.*;
import java.util.ArrayList;
import java.util.List;

public class Database {
    // Files to save and retrieve from, using separated files
    // *in case other related programs require one or multiple collections.
    static final String ITEM_FILE = "itemCollection.ser";
    static final String MEMBER_FILE = "memberCollection.ser";
    static final String USER_FILE = "userCollection.ser";

    // Variables
    ArrayList<Item> items;
    public List<Item> getItems() {
        return items;
    }

    ArrayList<Member> members;
    public List<Member> getMembers() {
        return members;
    }

    ArrayList<User> users;
    public List<User> getUsers() {
        return users;
    }

    // Constructor
    public Database() {
        try {
            items = readFile(ITEM_FILE);
            members = readFile(MEMBER_FILE);
            users = readFile(USER_FILE);
        }
        catch (Exception e) {
            new ErrorLogger().log(e);
        }
    }

    /**
     * Serializes all lists to their files.
     */
    public void saveToFiles() {
        // Try serializing all lists to file
        try{
            writeFile(ITEM_FILE, items);
            writeFile(MEMBER_FILE, members);
        }
        catch (Exception e) {
            new ErrorLogger().log(e);
        }
    }

    /**
     * Serializes list to a specified file.
     * @param itemFile Name of file.
     * @param list List to serialize.
     */
    <T> void writeFile(String itemFile, ArrayList<T> list) throws IOException {
        try (FileOutputStream fileOut = new FileOutputStream(itemFile);
             ObjectOutputStream objOut = new ObjectOutputStream(fileOut)) {
             objOut.writeObject(list);
        }
    }

    /**
     * Deserializes a list from a specified file.
     * @param itemFile Name of file.
     * @return Returns a Serializable list.
     */
    <T> ArrayList<T> readFile(String itemFile) {
        File file = new File(itemFile);
        if (file.exists() && !file.isDirectory()) {
            // Try reading file and returning the object as a List
            try (FileInputStream fileIn = new FileInputStream(itemFile);
                 ObjectInputStream objIn = new ObjectInputStream(fileIn)) {
                return (ArrayList<T>)objIn.readObject();
            }
            catch (Exception e) {
                // Log error
                new ErrorLogger().log(e);
                return new ArrayList<>();
            }
        }
        return new ArrayList<>();
    }
}
