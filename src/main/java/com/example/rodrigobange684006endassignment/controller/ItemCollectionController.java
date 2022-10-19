package com.example.rodrigobange684006endassignment.controller;

import com.example.rodrigobange684006endassignment.database.ItemDatabase;
import com.example.rodrigobange684006endassignment.model.Item;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.cell.PropertyValueFactory;

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

    // Database
    ItemDatabase itemDatabase;

    // Constructor
    public ItemCollectionController(ItemDatabase itemDatabase) {
        this.itemDatabase = itemDatabase;
    }

    // Initializer
    @Override
    public void initialize(URL url, ResourceBundle rb){
        // Set up the columns in the item collection tableview
        tblColItemCode.setCellValueFactory(new PropertyValueFactory<>("itemCode"));
        tblColItemAvailable.setCellValueFactory(new PropertyValueFactory<>("available"));
        tblColItemTitle.setCellValueFactory(new  PropertyValueFactory<>("title"));
        tblColItemAuthor.setCellValueFactory(new PropertyValueFactory<>("author"));

        // Load dummy data
        tblViewItems.setItems(itemDatabase.getItems());
    }
}
