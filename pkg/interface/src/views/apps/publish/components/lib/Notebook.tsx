import React from "react";
import { Link, RouteComponentProps, Route, Switch } from "react-router-dom";
import { NotebookPosts } from "./NotebookPosts";
import { Subscribers } from "./Subscribers";
import { Settings } from "./Settings";
import { roleForShip } from "~/logic/lib/group";
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
import { Notebook as INotebook } from "~/types/publish-update";
import { Groups } from "~/types/group-update";
import { Contacts, Rolodex } from "~/types/contact-update";
import GlobalApi from "~/logic/api/global";
import styled from "styled-components";
import {Association, Associations} from "~/types";
import {Graph} from "~/types/graph-update";

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
  graph: Graph;
  notebookContacts: Contacts;
  association: Association;
  associations: Associations;
  contacts: Rolodex;
  groups: Groups;
  hideNicknames: boolean;
}

export function Notebook(props: NotebookProps & RouteComponentProps) {
  const { api, ship, book, association, notebookContacts, groups } = props;

  const contact = notebookContacts[ship];
  const group = groups[association['group-path']];
  const role = group ? roleForShip(group, window.ship) : undefined;
  const isOwn = `~${window.ship}` === ship;
  const isAdmin = role === "admin" || isOwn;

  const isWriter =
    isOwn || group.tags?.publish?.[`writers-${book}`]?.has(window.ship);

  const showNickname = contact?.nickname && !props.hideNicknames;

  const { metadata } = props.association || {};

  return (
    <Box
      pt={4}
      mx="auto"
      display="grid"
      gridAutoRows="min-content"
      gridTemplateColumns={["100%", "1fr 1fr"]}
      maxWidth="500px"
      gridRowGap={[4, 6]}
      gridColumnGap={3}
    >
      <Box display={["block", "none"]} gridColumn={["1/2", "1/3"]}>
        <Link to="/~publish">{"<- All Notebooks"}</Link>
      </Box>
      <Box>
        <Text> {metadata?.title}</Text>
        <br />
        <Text color="lightGray">by </Text>
        <Text fontFamily={showNickname ? "sans" : "mono"}>
          {showNickname ? contact?.nickname : ship}
        </Text>
      </Box>
      <Row justifyContent={["flex-start", "flex-end"]}>
        {isWriter && (
          <Link to={`/~publish/notebook/ship/${ship}/${book}/new`}>
            <Button primary border>
              New Post
            </Button>
          </Link>
        )}
        {!isOwn && (
          <Button ml={isWriter ? 2 : 0} error border>
            Unsubscribe
          </Button>
        )}
      </Row>
      <Box gridColumn={["1/2", "1/3"]}>
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
                graph={props.graph}
                host={ship}
                book={book}
                contacts={notebookContacts}
                hideNicknames={props.hideNicknames}
              />
            </TabPanel>
            <TabPanel>
              <Box color="black">{metadata?.description}</Box>
            </TabPanel>
            <TabPanel>
              <Subscribers
                book={book}
                association={association}
                associations={props.associations}
                api={api}
                groups={groups}
                contacts={props.contacts}
              />
            </TabPanel>
            <TabPanel>
              <Settings
                host={ship}
                book={book}
                api={api}
                association={props.association}
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
