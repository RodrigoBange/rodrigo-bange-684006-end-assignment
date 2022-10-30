package com.example.rodrigobange684006endassignment.controller;

import com.example.rodrigobange684006endassignment.LibrarySystemApplication;
import com.example.rodrigobange684006endassignment.database.Database;
import com.example.rodrigobange684006endassignment.model.ErrorLogger;
import com.example.rodrigobange684006endassignment.model.Function;
import com.example.rodrigobange684006endassignment.model.Item;
import com.example.rodrigobange684006endassignment.service.CollectionService;
import javafx.collections.transformation.FilteredList;
import javafx.collections.transformation.SortedList;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.fxml.Initializable;
import javafx.scene.Scene;
import javafx.scene.control.*;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.paint.Color;
import javafx.stage.Stage;

import java.net.URL;
import java.util.ResourceBundle;

public class ItemCollectionController implements Initializable {
    @FXML
    TableView<Item> tblViewItems;
    @FXML
    TableColumn<Item, Integer> tblColItemCode;
    @FXML
    TableColumn<Item, Boolean> tblColItemAvailable;
    @FXML
    TableColumn<Item, String> tblColItemTitle;
    @FXML
    TableColumn<Item, String> tblColItemAuthor;
    @FXML
    Label lblWarning;
    @FXML
    TextField txtSearchBar;

    // Service
    CollectionService cService;

    // Dialog
    static final String ITEM_DIALOG = "collection-dialog.fxml";
    static final String DELETE_DIALOG = "delete-dialog.fxml";

    // Constructor
    public ItemCollectionController(Database database) {
        cService = new CollectionService(database);
    }

    // Initializer
    @Override
    public void initialize(URL url, ResourceBundle rb){
        // Set up the columns in the item collection tableview
        tblColItemCode.setCellValueFactory(new PropertyValueFactory<>("itemCode"));
        tblColItemAvailable.setCellValueFactory(new PropertyValueFactory<>("available"));
        tblColItemTitle.setCellValueFactory(new PropertyValueFactory<>("title"));
        tblColItemAuthor.setCellValueFactory(new PropertyValueFactory<>("author"));

        // Set Boolean to Yes/No display
        addAvailabilityFormatting();

        // Add search bar listener and filter
        addTableFiltering();
    }

    /**
     * Adds an availability formatter to the tableview column "available".
     */
    void addAvailabilityFormatting() {
        // Transfer Boolean to yes / no
        tblColItemAvailable.setCellFactory(col -> new TableCell<Item, Boolean>() {
            @Override
            protected void updateItem(Boolean available, boolean empty) {
                super.updateItem(available, empty);
                if (empty) {
                    setText(null);
                }
                else if (Boolean.TRUE.equals(available)) {
                    setText("Yes");
                }
                else {
                    setText("No");
                }
            }
        });
    }

    /**
     * Adds search bar functionality and table filtering with a sorted list.
     */
    void addTableFiltering() {
        // Create filter list
        FilteredList<Item> filteredList = new FilteredList<>(cService.getItems(), b -> true);

        // Add listener to textfield
        txtSearchBar.textProperty().addListener(((observableValue, oldValue, newValue) ->
                filteredList.setPredicate(item -> {
                    // If searchbar text is empty, display everything
                    if (newValue == null || newValue.isEmpty()) {
                        return true;
                    }
                    // Convert search text to lowercase
                    String lowerCaseFilter = newValue.toLowerCase();

                    // If filter matches title or author, else return false
                    if (item.getTitle().toLowerCase().contains(lowerCaseFilter)) {
                        return true;
                    }
                    else { return item.getAuthor().toLowerCase().contains(lowerCaseFilter); }
                })));

        // Put the filtered list in a sorted list
        SortedList<Item> sortedList = new SortedList<>(filteredList);

        // Bind the sorted list comparator to the tableview comparator
        // If this isn't applied, sorting in the tableview will not work
        sortedList.comparatorProperty().bind(tblViewItems.comparatorProperty());

        // Add the sorted list to the tableview
        tblViewItems.setItems(sortedList);
    }

    /**
     * Sets the warning message styling.
     * @param isPositive Whether the warning is critical or not. (Green or Red styling)
     * @param message Message to display with the corresponding warning.
     */
    void setWarningMessage(Boolean isPositive, String message) {
        if (Boolean.TRUE.equals(isPositive)) {
            lblWarning.setTextFill(Color.LIGHTGREEN);
        }
        else {
            lblWarning.setTextFill(Color.RED);
        }
        lblWarning.setText(message);
    }

    @FXML
    public void onAddItemClick() {
        // Attempt to add a new item
        addItem();
    }

    /**
     * Attempts to add a new item to the list.
     */
    void addItem() {
        try {
            // Initialize FXMLLoader and controller
            FXMLLoader fxmlLoader = new FXMLLoader(LibrarySystemApplication.class.getResource(ITEM_DIALOG));
            ItemDialogController itemDialogController = new ItemDialogController(cService, Function.ADD, null);
            fxmlLoader.setController(itemDialogController);

            // Initialize scene, stage and display dialog
            Scene scene = new Scene(fxmlLoader.load());
            Stage dialog = new Stage();
            dialog.setScene(scene);
            dialog.setTitle("Library System - Add Item");
            dialog.setResizable(false);
            dialog.showAndWait();

            // If window dialog closed and actually contains a new member, add it to the list
            if (itemDialogController.getItem() != null) {
                cService.addItem(itemDialogController.getItem());
                tblViewItems.refresh();
                setWarningMessage(true, "Successfully added new item.");
            }
            else { setWarningMessage(true, ""); }
        } catch (Exception e) {
            setWarningMessage(false, "An issue occurred trying to add the new item.");
            new ErrorLogger().log(e);
        }
    }

    @FXML
    public void onEditItemClick() {
        if (tblViewItems.getSelectionModel().getSelectedItem() != null) {
            // Get selected item
            Item selectedItem = tblViewItems.getSelectionModel().getSelectedItem();

            // Attempt to edit the item
            editItem(selectedItem);
        }
        else { setWarningMessage(false, "To edit, please select an item."); }
    }

    /**
     * Attempts to edit the selected item.
     * @param selectedItem // Selected item to edit.
     */
    void editItem(Item selectedItem) {
        try {
            // Initialize FXMLLoader and controller
            FXMLLoader fxmlLoader = new FXMLLoader(LibrarySystemApplication.class.getResource(ITEM_DIALOG));
            ItemDialogController itemDialogController = new ItemDialogController(cService, Function.EDIT, selectedItem);
            fxmlLoader.setController(itemDialogController);

            // Initialize scene, stage and display dialog
            Scene scene = new Scene(fxmlLoader.load());
            Stage dialog = new Stage();
            dialog.setScene(scene);
            dialog.setTitle("Library System - Edit Item");
            dialog.setResizable(false);
            dialog.showAndWait();

            // If window dialog closed and actually contains a new member, update it in the list
            if (Boolean.TRUE.equals(itemDialogController.getItemEdited())) {
                cService.updateItem(itemDialogController.getItem());
                tblViewItems.refresh();
                setWarningMessage(true, "Successfully updated item.");
            }
            else { setWarningMessage(true, ""); }
        } catch (Exception e) {
            setWarningMessage(false, "An issue occurred trying to edit the item.");
            new ErrorLogger().log(e);
        }
    }

    @FXML
    public void onDeleteItemClick() {
        if (tblViewItems.getSelectionModel().getSelectedItem() != null) {
            // Get selected item
            Item selectedItem = tblViewItems.getSelectionModel().getSelectedItem();

            // Attempt to delete the selected item
            deleteItem(selectedItem);
        }
        else {
            setWarningMessage(false, "To delete, please select an item.");
        }
    }

    /**
     * Attempts to delete the given item. (IF it is currently not being lent out.)
     * @param selectedItem The selected item to delete.
     */
    void deleteItem(Item selectedItem) {
        try {
            // Check if the item is lent out before allowing removal
            if (Boolean.FALSE.equals(cService.isItemLentOut(selectedItem.getItemCode()))) {
                // Initialize FXMLLoader and controller
                FXMLLoader fxmlLoader = new FXMLLoader(LibrarySystemApplication.class.getResource(DELETE_DIALOG));
                DeleteDialogController deleteDialogController = new DeleteDialogController(selectedItem.getTitle(),
                        "by " + selectedItem.getAuthor());
                fxmlLoader.setController(deleteDialogController);

                // Initialize scene, stage and display dialog
                Scene scene = new Scene(fxmlLoader.load());
                Stage dialog = new Stage();
                dialog.setScene(scene);
                dialog.setTitle("Library System - Delete item");
                dialog.setResizable(false);
                dialog.showAndWait();

                // Check if operation should continue
                if (Boolean.TRUE.equals(deleteDialogController.getConfirmDelete())) {
                    cService.removeItem(selectedItem);
                    tblViewItems.refresh();
                    setWarningMessage(true, "Successfully deleted the item.");
                }
                else { setWarningMessage(true, ""); }
            }
            else { setWarningMessage(false, "A lent out item can not be deleted."); }
        } catch (Exception e) {
            setWarningMessage(false, "An issue occurred trying to delete the item.");
            new ErrorLogger().log(e);
        }
    }
}
