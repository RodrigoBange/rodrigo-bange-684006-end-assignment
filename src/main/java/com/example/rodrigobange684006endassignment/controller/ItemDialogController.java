package com.example.rodrigobange684006endassignment.controller;

import com.example.rodrigobange684006endassignment.database.ItemDatabase;
import com.example.rodrigobange684006endassignment.model.Function;
import com.example.rodrigobange684006endassignment.model.Item;
import javafx.beans.property.StringProperty;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.stage.Stage;

import java.net.URL;
import java.util.ResourceBundle;

public class ItemDialogController implements Initializable {
    @FXML
    Label lblTitle;
    @FXML
    Label lblWarning;
    @FXML
    TextField txtTitle;
    @FXML
    TextField txtAuthor;
    @FXML
    Button btnFunction;

    // Database
    ItemDatabase itemDatabase;

    // Variables
    Item item;
    Boolean itemEdited = false;
    Function function;

    public Item getItem() { return item; }
    public Boolean getItemEdited() { return itemEdited; }

    // Constructor
    public ItemDialogController(ItemDatabase itemDatabase, Function function, Item item) {
        this.itemDatabase = itemDatabase;
        this.function = function;
        this.item = item;
    }

    @Override
    public void initialize(URL url, ResourceBundle resourceBundle) {
        if (function.equals(Function.ADD)) {
            btnFunction.setText("Add item");
        }
        else if (function.equals(Function.EDIT)) {
            lblTitle.setText("Edit existing item");
            btnFunction.setText("Apply changes");

            txtTitle.setText(item.getTitle());
            txtAuthor.setText(item.getAuthor());
        }
    }

    @FXML
    protected void onFunctionClick(ActionEvent event) {
        // If all fields are filled in...
        if (!txtTitle.getText().isEmpty() && !txtAuthor.getText().isEmpty()) {
            if (containsLetter(txtTitle.getText()) && containsLetter(txtAuthor.getText())) {
                // Get values
                String title = txtTitle.getText();
                String author = txtAuthor.getText();

                if (function.equals(Function.ADD)) {
                    addItem(title, author);
                }
                else if (function.equals(Function.EDIT)) {
                    editItem(title, author);
                    itemEdited = true;
                }

                // Close dialog window
                Stage stage = (Stage) ((Button) event.getSource()).getScene().getWindow();
                stage.close();
            }
            else {
                lblWarning.setText("Please enter valid values.");
            }
        }
        else {
            lblWarning.setText("Please fill in all the fields correctly.");
        }
    }

    Boolean containsLetter(String input) {
        // Check if input at least has a letter
        for (char c : input.toCharArray()) {
            if (Character.isAlphabetic(c)) {
                return true;
            }
        }
        return false;
    }

    void addItem(String title, String author) {
        // Set new item values
        item = new Item(itemDatabase.getItemHighestCode() + 1, title, author);
    }

    void editItem(String title, String author) {
        // Update item values
        item.setTitle(title);
        item.setAuthor(author);
    }

    @FXML
    protected void onCancelClick(ActionEvent event) {
        // Close dialog window
        Stage stage = (Stage) ((Button) event.getSource()).getScene().getWindow();
        stage.close();
    }

    @FXML
    protected void onAuthorTextChange(StringProperty observable, String oldValue, String newValue) {
        if (observable.getValue().length() > 0) {
            // Get the first character
            char firstChar = newValue.charAt(0);
            // Always check for valid input, due to backspace
            if (!Character.isAlphabetic(firstChar)) { // First letter must be alphabetic
                txtAuthor.setText(oldValue);
            }
            else { // Styling
                txtAuthor.setText(newValue.toUpperCase());
            }
            else {
                // Get last char
                char c = newValue.charAt(newValue.length() - 1);

                // If character is a digit, set to old value
                if (Character.isDigit(c)) {
                    txtAuthor.setText(oldValue);
                }
            }
        }
    }
}
