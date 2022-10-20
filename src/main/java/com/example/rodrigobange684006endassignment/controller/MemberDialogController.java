package com.example.rodrigobange684006endassignment.controller;

import com.example.rodrigobange684006endassignment.database.MemberDatabase;
import com.example.rodrigobange684006endassignment.model.Member;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.DatePicker;
import javafx.scene.control.TextField;

import java.net.URL;
import java.time.LocalDate;
import java.util.ResourceBundle;

public class MemberDialogController implements Initializable {
    @FXML
    TextField txtFirstname;
    @FXML
    TextField txtLastName;
    @FXML
    DatePicker dPickerBirthDate;

    // Database
    // TODO: Maybe not neccessary to call the database? But have to figure out a way to avoid adding an ID
    MemberDatabase memberDatabase;

    // Member
    Member member;

    public Member getMember() {
        return member;
    }

    // Constructor
    public MemberDialogController(MemberDatabase memberDatabase) {
        this.memberDatabase = memberDatabase;
    }

    @Override
    public void initialize(URL url, ResourceBundle resourceBundle) {

    }

    @FXML
    protected void onAddMemberClick() {
        // Get values out of textboxes
        String firstName = txtFirstname.getText();
        String lastName = txtLastName.getText();
        LocalDate dateOfBirth = dPickerBirthDate.getValue();

        //member = new Member()
    }
}
