package com.example.rodrigobange684006endassignment.database;

import com.example.rodrigobange684006endassignment.model.Member;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

import java.util.Date;

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
        // Check if a member exists with the entered Id
        for (Member member : members) {
            if (member.getIdentifier() == memberId)
            {
                return true;
            }
        }

        return false;
    }

    public ObservableList<Member> getMembers() {
        return members;
    }

    ObservableList<Member> dummyMembers() {
        return FXCollections.observableArrayList(
                new Member(1, "Rebecca","Michaelson", new Date()),
                new Member(2, "Elena", "Gilbert", new Date()),
                new Member(3, "Steven","Salvator", new Date()),
                new Member(4, "Caroline","Forbes", new Date()),
                new Member(5, "Matt","Eyre", new Date()),
                new Member(6, "Tyler","Blockwood", new Date()));
    }
}
