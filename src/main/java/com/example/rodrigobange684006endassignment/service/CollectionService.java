package com.example.rodrigobange684006endassignment.service;

import com.example.rodrigobange684006endassignment.database.Database;
import com.example.rodrigobange684006endassignment.model.Item;
import com.example.rodrigobange684006endassignment.model.Member;
import com.example.rodrigobange684006endassignment.model.ResultMessage;
import javafx.collections.ObservableList;

import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;

public class CollectionService {
    // Database
    Database database;

    // Variables
    ObservableList<Item> items;

    /**
     * Retrieves the list of items
     * @return Returns an Observable List of Item objects.
     */
    public ObservableList<Item> getItems() {
        return items;
    }

    // Constructor
    public CollectionService(Database database) {
        this.database = database;
    }

    /**
     * Adds an item to the item list.
     * @param itemToAdd Item to add.
     */
    public void addItem(Item itemToAdd) {
        int newCode;

        // Get the highest ID from the list
        if (!items.isEmpty()) {
            newCode = getItemHighestCode() + 1;
        }
        else {
            newCode = 1;
        }

        // Add the new item to the list
        items.add(new Item(newCode, itemToAdd.getTitle(), itemToAdd.getAuthor()));
    }

    /**
     * Removes an item from the item list.
     * @param itemToRemove Item to remove.
     */
    public void removeItem(Item itemToRemove) {
        // Remove the item if it exists in the list
        items.removeIf(item -> item.equals(itemToRemove));
    }

    /**
     * Updates an item from the item list.
     * @param itemToUpdate Item to update.
     */
    public void updateItem(Item itemToUpdate) {
        // Find item and update values
        for (Item item : items) {
            if (item.getItemCode() == itemToUpdate.getItemCode()) {
                item.setTitle(itemToUpdate.getTitle());
                item.setAuthor(itemToUpdate.getAuthor());
            }
        }
    }

    /**
     * Returns a Boolean depending on if the item exists.
     * @param itemCode Code of item to check.
     * @return Returns the result of the item existence as a Boolean.
     */
    public Boolean itemExists(int itemCode) {
        // Check if an item exists with the same item code
        for (Item item : items) {
            if (item.getItemCode() == itemCode) {
                return true;
            }
        }
        return false;
    }

    /**
     * Gets the highest code out of the item list.
     * @return Returns the highest code as an Integer.
     */
    public int getItemHighestCode() {
        // Get the last item and its code
        if (!items.isEmpty()) {
            return items.get(items.size() -1 ).getItemCode();
        }
        else {
            return 0;
        }
    }

    /**
     * Updates the give item to being lent out.
     * @param itemCode Item code of the item to be lent out.
     * @param memberId Member ID of the lender.
     * @return Returns the result with a Boolean and String message.
     */
    public ResultMessage updateLentItem(int itemCode, int memberId) {
        // Find the item and update it's lending status
        for (Item item : items) {
            if (item.getItemCode() == itemCode) {
                item.setAvailable(false);
                item.setLenderCode(memberId);
                item.setLendingDate(LocalDateTime.now());
                return new ResultMessage(true, "Item successfully lend out.");
            }
        }
        return new ResultMessage(false, "Item could not be lend out, ensure the information is correct.");
    }

    /**
     * Updates the lent out item to being returned.
     * @param itemCode Item code of the item to be returned.
     * @return Returns the result with a Boolean and String message.
     */
    public ResultMessage updateReceivedItem(int itemCode) {
        LocalDateTime currentDate = LocalDateTime.now();
        LocalDateTime lendingDate;

        // Update the received item to being available again and clear status
        for (Item item : items) {
            if (item.getItemCode() == itemCode) {
                // Check if item was returned in time
                lendingDate = item.getLendingDate();
                if (ChronoUnit.WEEKS.between(lendingDate, currentDate) >= 3) {
                    item.setAvailable(true);
                    item.setLendingDate(null);
                    item.setLenderCode(0);

                    return new ResultMessage(true, "WARNING: Item has been successfully returned but was" +
                            "returned too late by " + ChronoUnit.DAYS.between(lendingDate, currentDate) + " days.");
                }
                else {
                    return new ResultMessage(true, "Item has been successfully returned.");
                }
            }
        }

        // If item doesn't exist
        return new ResultMessage(false, "Item does not exist. Please ensure the information is correct.");
    }
}
