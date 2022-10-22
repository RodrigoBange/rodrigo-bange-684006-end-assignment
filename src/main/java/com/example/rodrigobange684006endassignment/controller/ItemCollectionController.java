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
import javafx.scene.control.Label;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.paint.Color;
import javafx.stage.Stage;

import java.io.IOException;
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
    String itemDialog = "collection-dialog.fxml";
    String deleteDialog = "delete-dialog.fxml";

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
        tblColItemTitle.setCellValueFactory(new  PropertyValueFactory<>("title"));
        tblColItemAuthor.setCellValueFactory(new PropertyValueFactory<>("author"));

        // Add search bar listener and filter
        addTableFiltering();
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

    @FXML
    public void onAddItemClick() {
        try {
            // Initialize FXMLLoader and controller
            FXMLLoader fxmlLoader = new FXMLLoader(LibrarySystemApplication.class.getResource(itemDialog));
            ItemDialogController itemDialogController = new ItemDialogController(cService, Function.ADD,
                    null);
            fxmlLoader.setController(itemDialogController);

            // Initialize scene and stage
            Scene scene = new Scene(fxmlLoader.load());
            Stage dialog = new Stage();

            // Display dialog
            dialog.setScene(scene);
            dialog.setTitle("Library System - Add Item");
            dialog.showAndWait();

            // If window dialog closed and actually contains a new member, add it to the list
            if (itemDialogController.getItem() != null) {
                cService.addItem(itemDialogController.getItem());
                tblViewItems.refresh();
                lblWarning.setTextFill(Color.LIGHTGREEN);
                lblWarning.setText("Successfully added new item.");
            }
            else { lblWarning.setText(""); }
        } catch (IOException e) {
            lblWarning.setTextFill(Color.RED);
            lblWarning.setText("An issue occurred trying to add the new item.");
            new ErrorLogger().log(e);
        }
    }

    @FXML
    public void onEditItemClick() {
        if (tblViewItems.getSelectionModel().getSelectedItem() != null){
            try {
                Item selectedItem = tblViewItems.getSelectionModel().getSelectedItem();

                // Initialize FXMLLoader and controller
                FXMLLoader fxmlLoader = new FXMLLoader(LibrarySystemApplication.class.getResource(itemDialog));
                ItemDialogController itemDialogController = new ItemDialogController(cService, Function.EDIT,
                        selectedItem);
                fxmlLoader.setController(itemDialogController);

                // Initialize scene and stage
                Scene scene = new Scene(fxmlLoader.load());
                Stage dialog = new Stage();

                // Display dialog
                dialog.setScene(scene);
                dialog.setTitle("Library System - Edit Item");
                dialog.showAndWait();

                // If window dialog closed and actually contains a new member, update it in the list
                if (Boolean.TRUE.equals(itemDialogController.getItemEdited())) {
                    cService.updateItem(itemDialogController.getItem());
                    tblViewItems.refresh();
                    lblWarning.setTextFill(Color.LIGHTGREEN);
                    lblWarning.setText("Successfully updated item.");
                }
                else { lblWarning.setText(""); }
            } catch (IOException e) {
                lblWarning.setTextFill(Color.RED);
                lblWarning.setText("An issue occurred trying to edit the item.");
                new ErrorLogger().log(e);
            }
        }
        else {
            // Display warning
            lblWarning.setTextFill(Color.RED);
            lblWarning.setText("To edit, please select an item.");
        }
    }

    @FXML
    public void onDeleteItemClick() {
        if (tblViewItems.getSelectionModel().getSelectedItem() != null) {
            try {
                Item selectedItem = tblViewItems.getSelectionModel().getSelectedItem();

                // Check if the item is lent out before allowing removal
                if (Boolean.FALSE.equals(cService.isItemLentOut(selectedItem.getItemCode()))) {
                    // Initialize FXMLLoader and controller
                    FXMLLoader fxmlLoader = new FXMLLoader(LibrarySystemApplication.class.getResource(deleteDialog));
                    DeleteDialogController deleteDialogController = new DeleteDialogController(selectedItem.getTitle(),
                            "by " + selectedItem.getAuthor());
                    fxmlLoader.setController(deleteDialogController);

                    // Initialize scene and stage
                    Scene scene = new Scene(fxmlLoader.load());
                    Stage dialog = new Stage();

                    // Display dialog
                    dialog.setScene(scene);
                    dialog.setTitle("Library System - Delete item");
                    dialog.showAndWait();

                    // Check if operation should continue
                    if (Boolean.TRUE.equals(deleteDialogController.confirmDelete)) {
                        cService.removeItem(selectedItem);
                        tblViewItems.refresh();
                        lblWarning.setTextFill(Color.LIGHTGREEN);
                        lblWarning.setText("Successfully deleted item.");
                    }
                    else { lblWarning.setText(""); }
                }
                else {
                    lblWarning.setTextFill(Color.RED);
                    lblWarning.setText("A lent out item can not be deleted.");
                }
            } catch (IOException e) {
                lblWarning.setTextFill(Color.RED);
                lblWarning.setText("An issue occurred trying to delete the item.");
                new ErrorLogger().log(e);
            }
        }
        else {
            lblWarning.setTextFill(Color.RED);
            lblWarning.setText("To delete, please select an item.");
        }
    }
}
