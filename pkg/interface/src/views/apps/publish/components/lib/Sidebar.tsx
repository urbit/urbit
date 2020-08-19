import React, { Component } from "react";
import { Box, Text, Col } from "@tlon/indigo-react";
import { Link } from "react-router-dom";
import SidebarInvite from "~/views/components/SidebarInvite";
import { Welcome } from "./Welcome";
import { GroupItem } from "./GroupItem";
import { alphabetiseAssociations } from "~/logic/lib/util";

export function Sidebar(props: any) {
  const sidebarInvites = !(props.invites && props.invites["/publish"])
    ? null
    : Object.keys(props.invites["/publish"]).map((uid) => {
        return (
          <SidebarInvite
            key={uid}
            invite={props.invites["/publish"][uid]}
            onAccept={() => props.api.invite.accept("/publish", uid)}
            onDecline={() => props.api.invite.decline("/publish", uid)}
          />
        );
      });
  const associations =
    props.associations && "contacts" in props.associations
      ? alphabetiseAssociations(props.associations.contacts)
      : {};

  const notebooks = {};
  Object.keys(props.notebooks).map((host) => {
    Object.keys(props.notebooks[host]).map((notebook) => {
      const title = `${host}/${notebook}`;
      notebooks[title] = props.notebooks[host][notebook];
    });
  });

  const groupedNotebooks = {};
  Object.keys(notebooks).map((book) => {
    const path = notebooks[book]["subscribers-group-path"]
      ? notebooks[book]["subscribers-group-path"]
      : book;
    if (path in associations) {
      if (groupedNotebooks[path]) {
        const array = groupedNotebooks[path];
        array.push(book);
        groupedNotebooks[path] = array;
      } else {
        groupedNotebooks[path] = [book];
      }
    } else {
      if (groupedNotebooks["/~/"]) {
        const array = groupedNotebooks["/~/"];
        array.push(book);
        groupedNotebooks["/~/"] = array;
      } else {
        groupedNotebooks["/~/"] = [book];
      }
    }
  });

  const selectedGroups = props.selectedGroups ? props.selectedGroups : [];
  const groupedItems = Object.keys(associations)
    .map((each, i) => {
      const books = groupedNotebooks[each] || [];
      if (books.length === 0) return;
      if (
        selectedGroups.length === 0 &&
        groupedNotebooks["/~/"] &&
        groupedNotebooks["/~/"].length !== 0
      ) {
        i = i + 1;
      }
      return (
        <GroupItem
          key={i}
          index={i}
          association={associations[each]}
          groupedBooks={books}
          notebooks={notebooks}
          path={props.path}
        />
      );
    });
  if (
    selectedGroups.length === 0 &&
    groupedNotebooks["/~/"] &&
    groupedNotebooks["/~/"].length !== 0
  ) {
    groupedItems.unshift(
      <GroupItem
        key={"/~/"}
        index={0}
        association={"/~/"}
        groupedBooks={groupedNotebooks["/~/"]}
        notebooks={notebooks}
        path={props.path}
      />
    );
  }

  const display = props.path ? ['none', 'block'] : 'block';

  return (
    <Col
      borderRight={[0, 1]}
      borderRightColor={["washedGray", "washedGray"]}
      height="100%"
      pt={[3, 0]}
      overflowY="auto"
      display={display}
      maxWidth={["none", "250px"]}
    >
      <Box>
        <Link to="/~publish/new" className="green2 pa4 f9 dib">
          <Box color="green">New Notebook</Box>
        </Link>
      </Box>
      <Box
        className="overflow-y-auto pb1"
      >
        <Welcome mx={2} />
        {sidebarInvites}
        {groupedItems}
      </Box>
    </Col>
  );
}

export default Sidebar;
