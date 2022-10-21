package com.example.rodrigobange684006endassignment.database;

import com.example.rodrigobange684006endassignment.model.Member;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

import java.time.LocalDate;

public class MemberDatabase extends Database<Member> {
    ObservableList<Member> members;

    // Constructor
    public MemberDatabase() {
        // Initialize list
        members = dummyMembers();
    }

    @Override
    public void add(Member member) {
        int newId;

        // Get the highest ID from the list
        if (!members.isEmpty()) {
            newId = members.get(members.size() - 1).getIdentifier() + 1;
        }
        else {
            newId = 1;
        }

        // Add the new member to the list
        members.add(new Member(newId, member.getFirstName(), member.getLastName(), member.getDateOfBirth()));
    }

    @Override
    public void remove(Member memberToRemove) {
        // Remove the member if it's the same as the member to be removed
        members.removeIf(member -> member.equals(memberToRemove));
    }

    @Override
    public void update(Member memberToUpdate) {
        // Update the member values
        for (Member member : members) {
            if (member.getIdentifier() == memberToUpdate.getIdentifier()) {
                member.setFirstName(memberToUpdate.getFirstName());
                member.setLastName(memberToUpdate.getLastName());
                member.setDateOfBirth(memberToUpdate.getDateOfBirth());
            }
        }
    }

    public Boolean memberExists(int memberId) {
        // Check if a member exists with the entered Identifier
        for (Member member : members) {
            if (member.getIdentifier() == memberId)
            {
                return true;
            }
        }
        return false;
    }

    public int getMemberHighestId() {
        // Get the last member and it's Identifier
        if (!members.isEmpty()) {
            return members.get(members.size() - 1).getIdentifier();
        }
        else {
            return 0;
        }
    }

    public ObservableList<Member> getMembers() {
        return members;
    }

    ObservableList<Member> dummyMembers() {
        return FXCollections.observableArrayList(
                new Member(1, "Rebecca","Michaelson", LocalDate.of(2000,2,19)),
                new Member(2, "Elena", "Gilbert", LocalDate.of(2000,2,19)),
                new Member(3, "Steven","Salvator", LocalDate.of(2000,2,19)),
                new Member(4, "Caroline","Forbes", LocalDate.of(2000,2,19)),
                new Member(5, "Matt","Eyre", LocalDate.of(2000,2,19)),
                new Member(6, "Tyler","Blockwood", LocalDate.of(2000,2,19)));
    }
}
