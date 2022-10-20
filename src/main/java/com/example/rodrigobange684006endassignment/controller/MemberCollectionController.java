package com.example.rodrigobange684006endassignment.controller;

import com.example.rodrigobange684006endassignment.LibrarySystemApplication;
import com.example.rodrigobange684006endassignment.database.MemberDatabase;
import com.example.rodrigobange684006endassignment.model.Member;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.fxml.Initializable;
import javafx.scene.Scene;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.stage.Stage;

import java.io.IOException;
import java.net.URL;
import java.util.Date;
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
    TableColumn<Member, Date> tblColMemberBirthDate;

    // Database
    MemberDatabase memberDatabase;

    // Dialog
    String addMemberDialog = "member-add-dialog.fxml";

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

        // Load dummy data
        tblViewMembers.setItems(memberDatabase.getMembers());
    }

    // Buttons
    @FXML
    public void onAddMemberClick() {
        try {
            // Initialize FXMLLoader and controller
            FXMLLoader fxmlLoader = new FXMLLoader(LibrarySystemApplication.class.getResource(addMemberDialog));
            MemberDialogController memberDialogController = new MemberDialogController(memberDatabase);
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
            }
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}
