package com.example.rodrigobange684006endassignment.database;

import com.example.rodrigobange684006endassignment.model.Item;
import com.example.rodrigobange684006endassignment.model.ResultMessage;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;

public class ItemDatabase extends Database<Item>{
    ObservableList<Item> items;

    // Constructor
    public ItemDatabase() {
        items = dummyItems();
    }

    @Override
    public void add(Item item) {
        int newId;

        // Get the highest ID from the list
        if (!items.isEmpty()) {
            newId = items.get(items.size() - 1).getItemCode() + 1;
        }
        else {
            newId = 1;
        }

        // Add the new item to the list
        items.add(new Item(newId, item.getTitle(), item.getAuthor()));
    }

    @Override
    public void remove(Item itemToRemove) {
        // Remove the item if it's the same as the item to be removed
        items.removeIf(item -> item.equals(itemToRemove));
    }

    @Override
    public void update(Item itemToUpdate) {
        // Update the item values
        for (Item item : items){
            if (item.getItemCode() == itemToUpdate.getItemCode()) {
                item.setTitle(itemToUpdate.getTitle());
                item.setAuthor(itemToUpdate.getAuthor());
            }
        }
    }

    public ResultMessage updateLendOutItem(int itemCode, int memberId) {
        // Update the item to being lent out and set the memberId to the item
        for (Item item : items) {
            if (item.getItemCode() == itemCode) {
                item.setAvailable(false);
                item.setLenderCode(memberId);
                item.setLendingDate(LocalDateTime.now());
                return new ResultMessage(true, "Item successfully lend out at " + item.getLendingDate() + ".");
            }
        }

        // If item doesn't exist
        return new ResultMessage(false, "Item could not be lend out. Please ensure the information is correct.");
    }

    public ResultMessage updateReceivedItem(int itemCode) {
        LocalDateTime currentDate = LocalDateTime.now();
        LocalDateTime lendDate;

        // Update the received item to being available again and remove the memberId
        for (Item item : items) {
            if (item.getItemCode() == itemCode) {
                // Check if lender returned item in time
                lendDate = item.getLendingDate();
                if (ChronoUnit.WEEKS.between(lendDate, currentDate) >= 3) {
                    item.setAvailable(true);
                    item.setLenderCode(0);
                    item.setLendingDate(null);

                    return new ResultMessage(true, "WARNING: Item has successfully been received but was" +
                                                "returned too late by " + ChronoUnit.DAYS.between(lendDate, currentDate) +
                                                " days.");

                }
                else {
                    return new ResultMessage(true, "Item has successfully been received at " +
                            LocalDateTime.now() + ".");
                }
            }
        }

        // If item doesn't exist
        return new ResultMessage(false, "Item could not be received. Please ensure the information is correct.");
    }

    public Boolean itemExists(int itemCode) {
        // Check if an item exists with the same code
        for (Item item : items) {
            if (item.getItemCode() == itemCode)
            {
                return true;
            }
        }
        return false;
    }

    public int getItemHighestCode() {
        // Get the last item and its code
        if (!items.isEmpty()) {
            return items.get(items.size() -1).getItemCode();
        }
        else {
            return 0;
        }
    }

    public ObservableList<Item> getItems() {
        return items;
    }

    ObservableList<Item> dummyItems() {
        return FXCollections.observableArrayList(
                new Item(1, true,"Java For Dummies, 1st edition", "Vries, E. de"),
                new Item(2, true,"Java For Dummies, 2nd edition", "Vries, E. de"),
                new Item(3, true,"Java For Dummies, 3rd edition", "Vries, E. de"),
                new Item(4, true,"Java For Dummies, 4th edition", "Vries, E. de"),
                new Item(5, true,"Java For Dummies, 5th edition", "Vries, E. de"),
                new Item(6, true,"Java For Dummies, 6th edition", "Vries, E. de"),
                new Item(7, true,"Java For Dummies, 7th edition", "Vries, E. de"),
                new Item(8, true,"Java For Dummies, 8th edition", "Vries, E. de"));
    }
}
