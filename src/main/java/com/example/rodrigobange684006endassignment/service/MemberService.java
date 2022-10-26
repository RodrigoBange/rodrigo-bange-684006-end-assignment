package com.example.rodrigobange684006endassignment.service;

import com.example.rodrigobange684006endassignment.database.Database;
import com.example.rodrigobange684006endassignment.model.Member;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

public class MemberService {
    // Database
    Database database;

    // Variables
    ObservableList<Member> members;

    /**
     * Retrieves the list of members
     * @return Returns an Observable List of Member objects.
     */
    public ObservableList<Member> getMembers() {
        return members;
    }

    // Constructor
    public MemberService (Database database) {
        this.database = database;
        members = FXCollections.observableList(database.getMembers());
    }

    /**
     * Adds a member to the member list.
     * @param memberToAdd Member to add.
     */
    public void addMember(Member memberToAdd) {
        int newId;

        // Get the highest ID from the list
        if (!members.isEmpty()) {
            newId = getMemberHighestId() + 1;
        }
        else {
            newId = 1;
        }

        // Add the new member to the list
        members.add(new Member(newId, memberToAdd.getFirstName(), memberToAdd.getLastName(), memberToAdd.getDateOfBirth()));
    }

    /**
     * Removes a member from the member list.
     * @param memberToRemove Member to remove.
     */
    public void removeMember(Member memberToRemove) {
        // Remove the member if it's the same as the member to be removed
        members.removeIf(member -> member.equals(memberToRemove));
    }

    /**
     * Updates a member from the member list.
     * @param memberToUpdate Member to update.
     */
    public void updateMember(Member memberToUpdate) {
        // Find member and update values
        for (Member member : members) {
            if (member == memberToUpdate) {
                member.setFirstName(memberToUpdate.getFirstName());
                member.setLastName(memberToUpdate.getLastName());
                member.setDateOfBirth(memberToUpdate.getDateOfBirth());
            }
        }
    }

    /**
     * Returns a Boolean depending on if the member exists.
     * @param memberId ID of member to check.
     * @return Returns the result of the member existence as a Boolean.
     */
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

    /**
     * Gets the highest ID out of the member list.
     * @return Returns the highest ID as an Integer.
     */
    public int getMemberHighestId() {
        // Get the last member and it's Identifier
        if (!members.isEmpty()) {
            return members.get(members.size() - 1).getIdentifier();
        }
        else {
            return 0;
        }
    }
}
