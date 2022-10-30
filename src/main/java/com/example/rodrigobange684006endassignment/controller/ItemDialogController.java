package com.example.rodrigobange684006endassignment.controller;

import com.example.rodrigobange684006endassignment.model.Function;
import com.example.rodrigobange684006endassignment.model.Item;
import com.example.rodrigobange684006endassignment.service.CollectionService;
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

    // Service
    CollectionService cService;

    // Variables
    Item item;
    Boolean itemEdited = false;
    Function function;

    public Item getItem() { return item; }
    public Boolean getItemEdited() { return itemEdited; }

    // Constructor
    public ItemDialogController(CollectionService cService, Function function, Item item) {
        this.cService = cService;
        this.function = function;
        this.item = item;
    }

    // Initializer
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

    /**
     * Attempts to either ADD or EDIT an item depending on the requested function.
     * The function was given upon initializing this controller
     */
    @FXML
    protected void onFunctionClick(ActionEvent event) {
        // If all fields are filled in...
        if (!txtTitle.getText().isEmpty() && !txtAuthor.getText().isEmpty()) {
            if (minimumRequirement(txtTitle.getText()) && minimumRequirement(txtAuthor.getText())) {
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

    /**
     * Checks if the input has at least two characters.
     * @param input Input String to check.
     * @return Returns a boolean depending on if the value is valid.
     */
    Boolean minimumRequirement(String input) {
        // Check if input starts with a letter and has at least 2 characters
        char c = input.charAt(0);
        return Character.isAlphabetic(c) && input.length() > 1;
    }

    /**
     * Attempts to add a new item to the list.
     * @param title Title of the item.
     * @param author Author of the item.
     */
    void addItem(String title, String author) {
        // Set new item values
        item = new Item(cService.getItemHighestCode() + 1, title, author);
    }

    /**
     * Attempts to edit the item.
     * @param title Title of the item.
     * @param author Author of the item.
     */
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

    /**
     * Sets the first letter to always be capitalized for styling and user ease.
     */
    @FXML
    protected void onTitleTextChange(StringProperty observable, String oldValue, String newValue) {
        if (observable.getValue().length() > 0) {
            // Set first letter to be capitalized
            txtTitle.setText(newValue.substring(0,1).toUpperCase() + newValue.substring(1));
        }
    }

    /**
     * Sets the first letter to always be capitalized for styling and user ease and removes any digits.
     */
    @FXML
    protected void onAuthorTextChange(StringProperty observable, String oldValue, String newValue) {
        if (observable.getValue().length() > 0) {
            // Check for digits and remove them
            char[] chars = newValue.toCharArray();
            for (char c : chars) {
                if (Character.isDigit(c)) {
                    txtAuthor.setText(oldValue);
                    return;
                }
            }
            // Set first letter to be capitalized
            txtAuthor.setText(newValue.substring(0,1).toUpperCase() + newValue.substring(1));
        }
    }
}
