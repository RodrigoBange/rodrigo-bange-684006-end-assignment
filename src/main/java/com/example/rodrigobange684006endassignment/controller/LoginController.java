package com.example.rodrigobange684006endassignment.controller;

import com.example.rodrigobange684006endassignment.LibrarySystemApplication;
import com.example.rodrigobange684006endassignment.database.Database;
import com.example.rodrigobange684006endassignment.model.ErrorLogger;
import com.example.rodrigobange684006endassignment.model.ResultMessage;
import com.example.rodrigobange684006endassignment.service.UserService;
import javafx.beans.property.StringProperty;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.fxml.Initializable;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.stage.Stage;

import java.io.IOException;
import java.net.URL;
import java.util.ResourceBundle;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class LoginController implements Initializable {
    @FXML
    TextField txtFieldUsername;
    @FXML
    TextField txtFieldPassword;
    @FXML
    Label lblErrorMessage;
    @FXML
    Label lblTitleErrorMessage;
    @FXML
    Button btnLogIn;

    // Service
    UserService uService;
    // Database
    Database database;

    // Variables
    static final String MAIN_VIEW = "main-view.fxml";
    // Requirements password: At least 1 digit + 1 lower char + 1 upper char + 1 special char + 8 char long
    static final String PASSWORD_PATTERN =
            "^(?=.*[0-9])(?=.*[a-z])(?=.*[A-Z])(?=.*[!@#&()–[{}]:;',?/*~$^+=<>]).{8,}$";
    static final Pattern pattern = Pattern.compile(PASSWORD_PATTERN);
    Boolean validPassword = false;
    Boolean validUsername = false;

    // Initializer
    @Override
    public void initialize(URL url, ResourceBundle resourceBundle) {
        database = new Database();
        uService = new UserService(database);

        // Check if user file is present, if not, display message and disable text fields
        if (Boolean.FALSE.equals(uService.isLoginEnabled())) {
            lblTitleErrorMessage.setText("The login services are currently disabled." +
                    "\nPlease try again later. (Users file is missing)");
            txtFieldUsername.setDisable(true);
            txtFieldPassword.setDisable(true);
        }
    }

    /**
     * Enables the login button if both the username and password comply to the rules.
     */
    void enableLogInButton() {
        // If all requirements are met, enable log in button
        btnLogIn.setDisable(!validUsername || !validPassword);
    }

    /**
     * Checks if the given password complies to the password pattern rules.
     * @param password Password to check.
     * @return Returns a Boolean depending on if a valid password has been entered.
     */
    Boolean isValidPassword(String password){
        // Check if password matches the pattern requirements
        Matcher matcher = pattern.matcher(password);
        return matcher.matches();
    }

    @FXML
    protected void logInButtonClick() {
        String username = txtFieldUsername.getText();
        String password = txtFieldPassword.getText();

        checkCredentials(username, password);
    }

    /**
     * Attempts to log the user in by checking their credentials.
     * @param username Entered username to check.
     * @param password Entered password to check.
     */
    void checkCredentials(String username, String password) {
        // See if values match any from database...
        ResultMessage result = uService.validateLogin(username, password);

        // Try logging in
        if (Boolean.TRUE.equals(result.getResult())) {
            // Open the main application
            try {
                switchToMainScene(result.getMessage());
            }
            catch (Exception e) {
                new ErrorLogger().log(e);
                lblErrorMessage.setText("An error occurred retrieving user information.");
            }
        }
        else {
            lblErrorMessage.setText(result.getMessage());
        }
    }

    /**
     * Tries to open the main view and passes the users first and last name for display.
     * @param displayName Full name to display on the main view.
     */
    void switchToMainScene(String displayName) throws IOException {
        // Initialize stage
        Stage stage = new Stage();

        // Initialize FXMLLoader and controller
        FXMLLoader fxmlLoader = new FXMLLoader(LibrarySystemApplication.class.getResource(MAIN_VIEW));
        MainController mainController = new MainController(database, stage, displayName);
        fxmlLoader.setController(mainController);

        // Initialize scene and display dashboard
        Scene scene = new Scene(fxmlLoader.load());
        stage.setScene(scene);
        stage.setTitle("Library System - Dashboard");
        stage.sizeToScene();
        stage.show();
        stage.setMinWidth(stage.getWidth());
        stage.setMinHeight(stage.getHeight());

        // Close current window
        Stage currentStage = (Stage)btnLogIn.getScene().getWindow();
        currentStage.close();
    }

    /**
     * Removes all spaces within the username TextField.
     */
    @FXML
    protected void onUsernameTextChange(StringProperty observable, String oldValue, String newValue) {
        // Remove spaces
        if (observable.getValue().length() > 0) {
            // Check for spaces and remove them
            char[] chars = newValue.toCharArray();
            for (char c : chars) {
                if (c == ' ') {
                    txtFieldUsername.setText(oldValue);
                }
            }
        }

        // If field is not empty set to true
        validUsername = observable.getValue().length() > 0;

        // Check if login button can be enabled
        enableLogInButton();
    }

    /**
     * Removes all spaces within the password TextField.
     */
    @FXML
    protected void onPasswordTextChange(StringProperty observable, String oldValue, String newValue) {
        // Remove spaces
        if (observable.getValue().length() > 0) {
            // Check for spaces and remove them
            char[] chars = newValue.toCharArray();
            for (char c : chars) {
                if (c == ' ') {
                    txtFieldUsername.setText(oldValue);
                }
            }
        }

        // Check for password validity
        validPassword = isValidPassword(observable.getValue());

        if (Boolean.FALSE.equals(validPassword) && observable.getValue().length() > 0) {
            lblErrorMessage.setText("Password does not match the criteria.");
        }
        else {
            lblErrorMessage.setText("");
        }

        // Check if login button can be enabled
        enableLogInButton();
    }
}