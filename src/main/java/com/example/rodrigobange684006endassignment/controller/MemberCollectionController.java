package com.example.rodrigobange684006endassignment.controller;

import com.example.rodrigobange684006endassignment.LibrarySystemApplication;
import com.example.rodrigobange684006endassignment.database.MemberDatabase;
import com.example.rodrigobange684006endassignment.model.Function;
import com.example.rodrigobange684006endassignment.model.Member;
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

import java.io.IOException;
import java.net.URL;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.Locale;
import java.util.ResourceBundle;

public class MemberCollectionController implements Initializable {
    @FXML
    TableView<Member> tblViewMembers;
    @FXML
    TableColumn<Member, Integer> tblColMemberIdentifier;
    @FXML
    TableColumn<Member, String> tblColMemberFirstName;
    @FXML
    TableColumn<Member, String> tblColMemberLastName;
    @FXML
    TableColumn<Member, LocalDate> tblColMemberBirthDate;
    @FXML
    Label lblWarning;
    @FXML
    TextField txtSearchBar;

    // Database
    MemberDatabase memberDatabase;

    // Dialog
    String addMemberDialog = "member-dialog.fxml";

    final DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy");

    // Constructor
    public MemberCollectionController(MemberDatabase memberDatabase) {
        this.memberDatabase = memberDatabase;
    }

    // Initializer
    @Override
    public void initialize(URL url, ResourceBundle rb) {
        // Set up the columns in the member collection tableview
        tblColMemberIdentifier.setCellValueFactory(new PropertyValueFactory<>("identifier"));
        tblColMemberFirstName.setCellValueFactory(new PropertyValueFactory<>("firstName"));
        tblColMemberLastName.setCellValueFactory(new PropertyValueFactory<>("lastName"));
        tblColMemberBirthDate.setCellValueFactory(new PropertyValueFactory<>("dateOfBirth"));

        // Set date format of table
        addDateFormatter();

        // Add search bar listener and filter
        addTableFiltering();
    }

    /**
     * Adds a formatter to the tableview date.
     */
    void addDateFormatter() {
        // Add date formatter to table
        tblColMemberBirthDate.setCellFactory(col -> new TableCell<Member, LocalDate>() {
            @Override
            protected void updateItem(LocalDate date, boolean empty) {
                super.updateItem(date, empty);
                if (empty) {
                    setText(null);
                }
                else {
                    setText(String.format(date.format(formatter)));
                }
            }
        });
    }

    /**
     * Adds search bar functionality and table filtering with a sorted list.
     */
    void addTableFiltering() {
        // Create filter list
        FilteredList<Member> filteredList = new FilteredList<>(memberDatabase.getMembers(), b -> true);

        // Add listener to textfield
        txtSearchBar.textProperty().addListener(((observableValue, oldValue, newValue) -> {
            filteredList.setPredicate(member -> {
                // If searchbar text is empty, display everything
                if (newValue == null || newValue.isEmpty()) {
                    return true;
                }

                // Convert search text to lowercase
                String lowerCaseFilter = newValue.toLowerCase();

                // If filter matches first name or last name
                if (member.getFirstName().toLowerCase().contains(lowerCaseFilter)) {
                    return true;
                }
                else if (member.getLastName().toLowerCase().contains(lowerCaseFilter)) {
                    return true;
                }
                else { return false; } // Does not match
            });
        }));

        // Put the filtered list in a sorted list
        SortedList<Member> sortedList = new SortedList<>(filteredList);

        // Bind the sorted list comparator to the tableview comparator
        // If this isn't applied, sorting in the tableview will not work
        sortedList.comparatorProperty().bind(tblViewMembers.comparatorProperty());

        // Add the sorted list to the tableview
        tblViewMembers.setItems(sortedList);
    }

    // Buttons
    @FXML
    public void onAddMemberClick() {
        try {
            // Initialize FXMLLoader and controller
            FXMLLoader fxmlLoader = new FXMLLoader(LibrarySystemApplication.class.getResource(addMemberDialog));
            MemberDialogController memberDialogController = new MemberDialogController(memberDatabase, Function.ADD,
                                                                                null);
            fxmlLoader.setController(memberDialogController);

            // Initialize scene and stage
            Scene scene = new Scene(fxmlLoader.load());
            Stage dialog = new Stage();

            // Display dialog
            dialog.setScene(scene);
            dialog.setTitle("Library System - Add Member");
            dialog.showAndWait();

            // If window dialog closed and actually contains a new member, add it to the list
            if (memberDialogController.getMember() != null) {
                memberDatabase.add(memberDialogController.getMember());
                tblViewMembers.refresh();
                lblWarning.setTextFill(Color.LIGHTGREEN);
                lblWarning.setText("Successfully added new member");
            }
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    @FXML
    public void onEditMemberClick() {
        if (tblViewMembers.getSelectionModel().getSelectedItem() != null){
            try {
                Member selectedMember = tblViewMembers.getSelectionModel().getSelectedItem();

                // Initialize FXMLLoader and controller
                FXMLLoader fxmlLoader = new FXMLLoader(LibrarySystemApplication.class.getResource(addMemberDialog));
                MemberDialogController memberDialogController = new MemberDialogController(memberDatabase, Function.EDIT,
                                                                                            selectedMember);
                fxmlLoader.setController(memberDialogController);

                // Initialize scene and stage
                Scene scene = new Scene(fxmlLoader.load());
                Stage dialog = new Stage();

                // Display dialog
                dialog.setScene(scene);
                dialog.setTitle("Library System - Edit Member");
                dialog.showAndWait();

                // If window dialog closed and actually contains a new member, update it in the list
                if (Boolean.TRUE.equals(memberDialogController.getMemberEdited())) {
                    memberDatabase.update(memberDialogController.getMember());
                    tblViewMembers.refresh();
                    lblWarning.setTextFill(Color.LIGHTGREEN);
                    lblWarning.setText("Successfully updated member");
                }
                else { lblWarning.setText(""); }
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }
        else {
            // Display warning
            lblWarning.setTextFill(Color.RED);
            lblWarning.setText("Please select a member");
        }
    }

    @FXML
    public void onDeleteMemberClick() {
        if (tblViewMembers.getSelectionModel().getSelectedItem() != null) {
            Member selectedMember = tblViewMembers.getSelectionModel().getSelectedItem();

            // Remove member from list
            memberDatabase.remove(selectedMember);
            tblViewMembers.refresh();
            lblWarning.setTextFill(Color.LIGHTGREEN);
            lblWarning.setText("Successfully deleted member");
        }
        else {
            lblWarning.setTextFill(Color.RED);
            lblWarning.setText("Please select a member");
        }
    }
}
