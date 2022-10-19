package com.example.rodrigobange684006endassignment.controller;

import com.example.rodrigobange684006endassignment.database.MemberDatabase;
import com.example.rodrigobange684006endassignment.model.Member;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.cell.PropertyValueFactory;

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
}
