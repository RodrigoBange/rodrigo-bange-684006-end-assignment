package com.example.rodrigobange684006endassignment.controller;

import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.stage.Stage;

import java.net.URL;
import java.util.ResourceBundle;

public class DeleteDialogController implements Initializable {
    @FXML
    Label lblMainValue;
    @FXML
    Label lblSubValue;

    // Variables
    String mainValue;
    String subValue;
    Boolean confirmDelete = false;

    public Boolean getConfirmDelete() {
        return confirmDelete;
    }

    // Constructor
    public DeleteDialogController(String mainValue, String subValue) {
        this.mainValue = mainValue;
        this.subValue = subValue;
    }

    // Initializer
    @Override
    public void initialize(URL url, ResourceBundle resourceBundle) {
        // Set text to labels
        lblMainValue.setText(mainValue);
        lblSubValue.setText(subValue);
    }

    /**
     * Sets the Boolean for deletion to be true.
     */
    @FXML
    protected void onConfirmClick(ActionEvent event) {
        confirmDelete = true;
        // Close dialog window
        Stage stage = (Stage) ((Button) event.getSource()).getScene().getWindow();
        stage.close();
    }

    @FXML
    protected void onCancelClick(ActionEvent event) {
        // Close dialog window
        Stage stage = (Stage) ((Button) event.getSource()).getScene().getWindow();
        stage.close();
    }
}
