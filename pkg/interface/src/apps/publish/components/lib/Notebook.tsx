import React, { Component, useEffect } from "react";
import { Link, RouteComponentProps, Route, Switch } from "react-router-dom";
import { SidebarSwitcher } from "../../../../components/SidebarSwitch";
import { NotebookPosts } from "./NotebookPosts";
import { Subscribers } from "./subscribers";
import { Settings } from "./Settings";
import { cite } from "../../../../lib/util";
import { roleForShip } from "../../../../lib/group";
import { PublishContent } from "./PublishContent";
import {
  Box,
  Button,
  Text,
  Tab as _Tab,
  Tabs,
  TabList as _TabList,
  TabPanels,
  TabPanel,
  Row,
} from "@tlon/indigo-react";
import { Notebook as INotebook } from "../../../../types/publish-update";
import { Groups } from "../../../../types/group-update";
import { Contacts, Rolodex } from "../../../../types/contact-update";
import GlobalApi from "../../../../api/global";
import styled from "styled-components";

const TabList = styled(_TabList)`
  margin-bottom: ${(p) => p.theme.space[4]}px;
`;

const Tab = styled(_Tab)`
  flex-grow: 1;
`;

interface NotebookProps {
  api: GlobalApi;
  ship: string;
  book: string;
  notebook: INotebook;
  notebookContacts: Contacts;
  contacts: Rolodex;
  groups: Groups;
}

export function Notebook(props: NotebookProps & RouteComponentProps) {
  const { api, ship, book, notebook, notebookContacts, groups } = props;

  useEffect(() => {
    api.publish.fetchNotesPage(ship, book, 1, 50);
    api.publish.fetchNotebook(ship, book);
  }, [ship, book, api]);

  const contact = notebookContacts[ship];
  const group = groups[notebook?.["writers-group-path"]];

  const role = group ? roleForShip(group, window.ship) : undefined;

  const isOwn = `~${window.ship}` === ship;
  const isAdmin = role === "admin" || isOwn;

  const isWriter =
    isOwn || group.tags?.publish?.[`writers-${book}`]?.has(window.ship);

  const notesList = notebook?.["notes-by-date"] || [];
  const notes = notebook?.notes || {};

  return (
    <Box
      pt={4}
      display="grid"
      gridAutoRows="min-content"
      gridTemplateColumns="1fr 1fr"
      width="100%"
      maxWidth="500px"
      gridRowGap={6}
      gridColumnGap={3}
    >
      <Box>
        {notebook?.title}
        <br />
        <Text color="lightGray">by </Text>
        <Text fontFamily={contact?.nickname ? "sans" : "mono"}>
          {contact?.nickname || ship}
        </Text>
      </Box>
      <Row justifyContent="flex-end">
        {isWriter && (
          <Link to={`/~publish/notebook/${ship}/${book}/new`}>
            <Button primary border>
              New Post
            </Button>
          </Link>
        )}
        {!isOwn && (
          <Button ml={2} error border>
            Unsubscribe
          </Button>
        )}
      </Row>
      <Box gridColumn="1/4">
        <Tabs>
          <TabList>
            <Tab>All Posts</Tab>
            <Tab>About</Tab>
            {isAdmin && <Tab>Subscribers</Tab>}
            {isOwn && <Tab>Settings</Tab>}
          </TabList>
          <TabPanels>
            <TabPanel>
              <NotebookPosts
                notes={notes}
                list={notesList}
                host={ship}
                book={book}
                contacts={notebookContacts}
              />
            </TabPanel>
            <TabPanel>
              <Box>{notebook?.about}</Box>
            </TabPanel>
            <TabPanel>
              <Subscribers
                host={ship}
                book={book}
                notebook={notebook}
                api={api}
                groups={groups}
              />
            </TabPanel>
            <TabPanel>
              <Settings
                host={ship}
                book={book}
                api={api}
                notebook={notebook}
                contacts={notebookContacts}
              />
            </TabPanel>
          </TabPanels>
        </Tabs>
      </Box>
    </Box>
  );
}

export default Notebook;
