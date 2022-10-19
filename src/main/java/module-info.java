module com.example.rodrigobange684006endassignment {
    requires javafx.controls;
    requires javafx.fxml;


    opens com.example.rodrigobange684006endassignment to javafx.fxml;
    exports com.example.rodrigobange684006endassignment;

    exports com.example.rodrigobange684006endassignment.model;
    exports com.example.rodrigobange684006endassignment.database;
    exports com.example.rodrigobange684006endassignment.controller;
    opens com.example.rodrigobange684006endassignment.controller to javafx.fxml;
}