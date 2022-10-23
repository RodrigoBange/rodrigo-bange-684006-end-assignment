package com.example.rodrigobange684006endassignment.controller;

import com.example.rodrigobange684006endassignment.LibrarySystemApplication;
import com.example.rodrigobange684006endassignment.database.Database;
import com.example.rodrigobange684006endassignment.model.ErrorLogger;
import com.example.rodrigobange684006endassignment.model.Function;
import com.example.rodrigobange684006endassignment.model.Member;
import com.example.rodrigobange684006endassignment.service.CollectionService;
import com.example.rodrigobange684006endassignment.service.MemberService;
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

    // Service
    MemberService mService;
    CollectionService cService;

    // Dialog
    String memberDialog = "member-dialog.fxml";
    String deleteDialog = "delete-dialog.fxml";

    final DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd-MM-yyyy");

    // Constructor
    public MemberCollectionController(Database database) {
        mService = new MemberService(database);
        cService = new CollectionService(database);
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
        FilteredList<Member> filteredList = new FilteredList<>(mService.getMembers(), b -> true);

        // Add listener to textfield
        txtSearchBar.textProperty().addListener(((observableValue, oldValue, newValue) ->
            filteredList.setPredicate(member -> {
            // If searchbar text is empty, display everything
            if (newValue == null || newValue.isEmpty()) {
                return true;
            }

            // Convert search text to lowercase
            String lowerCaseFilter = newValue.toLowerCase();

            // If filter matches first name or last name, else return false
            if (member.getFirstName().toLowerCase().contains(lowerCaseFilter)) {
                return true;
            }
            else { return member.getLastName().toLowerCase().contains(lowerCaseFilter); }
        })));

        // Put the filtered list in a sorted list
        SortedList<Member> sortedList = new SortedList<>(filteredList);

        // Bind the sorted list comparator to the tableview comparator
        // If this isn't applied, sorting in the tableview will not work
        sortedList.comparatorProperty().bind(tblViewMembers.comparatorProperty());

        // Add the sorted list to the tableview
        tblViewMembers.setItems(sortedList);
    }

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
    public void onAddMemberClick() {
        // Attempt to add new member
        addMember();
    }

    void addMember() {
        try {
            // Initialize FXMLLoader and controller
            FXMLLoader fxmlLoader = new FXMLLoader(LibrarySystemApplication.class.getResource(memberDialog));
            MemberDialogController memberDialogController = new MemberDialogController(mService, Function.ADD, null);
            fxmlLoader.setController(memberDialogController);

            // Initialize scene and stage and display dialog
            Scene scene = new Scene(fxmlLoader.load());
            Stage dialog = new Stage();
            dialog.setScene(scene);
            dialog.setTitle("Library System - Add Member");
            dialog.showAndWait();

            // If window dialog closed and actually contains a new member, add it to the list
            if (memberDialogController.getMember() != null) {
                mService.addMember(memberDialogController.getMember());
                tblViewMembers.refresh();
                setWarningMessage(true, "Successfully added new member.");
            }
            else { setWarningMessage(true, ""); }
        } catch (IOException e) {
            setWarningMessage(false, "An issue occurred trying to add the new member.");
            new ErrorLogger().log(e);
        }
    }

    @FXML
    public void onEditMemberClick() {
        if (tblViewMembers.getSelectionModel().getSelectedItem() != null){
            // Get the selected member
            Member selectedMember = tblViewMembers.getSelectionModel().getSelectedItem();

            // Attempt to edit the member
            editMember(selectedMember);
        }
        else {
            // Display warning
            setWarningMessage(false, "To edit, please select a member.");
        }
    }

    void editMember(Member selectedMember) {
        try {
            // Initialize FXMLLoader and controller
            FXMLLoader fxmlLoader = new FXMLLoader(LibrarySystemApplication.class.getResource(memberDialog));
            MemberDialogController memberDialogController = new MemberDialogController(mService, Function.EDIT,
                    selectedMember);
            fxmlLoader.setController(memberDialogController);

            // Initialize scene, stage and display dialog
            Scene scene = new Scene(fxmlLoader.load());
            Stage dialog = new Stage();
            dialog.setScene(scene);
            dialog.setTitle("Library System - Edit Member");
            dialog.showAndWait();

            // If window dialog closed and actually contains a new member, update it in the list
            if (Boolean.TRUE.equals(memberDialogController.getMemberEdited())) {
                mService.updateMember(memberDialogController.getMember());
                tblViewMembers.refresh();
                setWarningMessage(true, "Successfully updated member.");
            }
            else { setWarningMessage(true, ""); }
        } catch (IOException e) {
            setWarningMessage(false, "An issue occurred trying to edit the member.");
            new ErrorLogger().log(e);
        }
    }

    @FXML
    public void onDeleteMemberClick() {
        if (tblViewMembers.getSelectionModel().getSelectedItem() != null) {
            // Get the selected member
            Member selectedMember = tblViewMembers.getSelectionModel().getSelectedItem();

            // Attempt to delete the member
            deleteMember(selectedMember);
        }
        else {
            setWarningMessage(false, "To delete, please select a member.");
        }
    }

    void deleteMember(Member selectedMember) {
        try {
            // Check if the member is not lending something before allowing removal
            if (Boolean.FALSE.equals(cService.isMemberLending(selectedMember.getIdentifier()))) {
                // Initialize FXMLLoader and controller
                FXMLLoader fxmlLoader = new FXMLLoader(LibrarySystemApplication.class.getResource(deleteDialog));
                DeleteDialogController deleteDialogController = new DeleteDialogController((selectedMember.getFirstName() +
                        " " + selectedMember.getLastName()), selectedMember.getDateOfBirth().format(formatter));
                fxmlLoader.setController(deleteDialogController);

                // Initialize scene, stage and dialog
                Scene scene = new Scene(fxmlLoader.load());
                Stage dialog = new Stage();
                dialog.setScene(scene);
                dialog.setTitle("Library System - Delete member");
                dialog.showAndWait();

                // Check if operation should continue
                if (Boolean.TRUE.equals(deleteDialogController.confirmDelete)) {
                    mService.removeMember(selectedMember);
                    tblViewMembers.refresh();
                    setWarningMessage(true, "Successfully deleted the member.");
                }
                else { setWarningMessage(true, ""); }
            }
            else { setWarningMessage(false, "A member with a lent item can not be deleted."); }
        } catch (IOException e) {
            setWarningMessage(false, "An issue occurred trying to delete the member.");
            new ErrorLogger().log(e);
        }
    }
}
