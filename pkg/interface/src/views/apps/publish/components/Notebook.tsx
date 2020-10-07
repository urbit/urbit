import React, { PureComponent } from "react";
import { Link, RouteComponentProps, Route, Switch } from "react-router-dom";
import { NotebookPosts } from "./NotebookPosts";
import { Subscribers } from "./Subscribers";
import { Settings } from "./Settings";
import { Spinner } from "~/views/components/Spinner";
import { Tabs, Tab } from "~/views/components/Tab";
import { roleForShip } from "~/logic/lib/group";
import { Box, Button, Text, Row } from "@tlon/indigo-react";
import { Notebook as INotebook } from "~/types/publish-update";
import { Groups } from "~/types/group-update";
import { Contacts, Rolodex } from "~/types/contact-update";
import GlobalApi from "~/logic/api/global";
import styled from "styled-components";
import { Associations } from "~/types";
import { deSig } from "~/logic/lib/util";

interface NotebookProps {
  api: GlobalApi;
  ship: string;
  book: string;
  notebook: INotebook;
  notebookContacts: Contacts;
  contacts: Rolodex;
  groups: Groups;
  hideNicknames: boolean;
  baseUrl: string;
  rootUrl: string;
  associations: Associations;
}

interface NotebookState {
  isUnsubscribing: boolean;
  tab: string;
}

export class Notebook extends PureComponent<
  NotebookProps & RouteComponentProps,
  NotebookState
> {
  constructor(props) {
    super(props);
    this.state = {
      isUnsubscribing: false,
      tab: "all",
    };
    this.setTab = this.setTab.bind(this);
  }

  setTab(tab: string) {
    this.setState({ tab });
  }

  render() {
    const {
      api,
      ship,
      book,
      notebook,
      notebookContacts,
      groups,
      history,
      hideNicknames,
      associations,
    } = this.props;
    const { state } = this;

    const group = groups[notebook?.["writers-group-path"]];
    if (!group) return null; // Waitin on groups to populate

    const relativePath = (p: string) => this.props.baseUrl + p;

    const contact = notebookContacts?.[ship];
    const role = group ? roleForShip(group, window.ship) : undefined;
    const isOwn = `~${window.ship}` === ship;
    const isAdmin = role === "admin" || isOwn;

    const isWriter =
      isOwn || group.tags?.publish?.[`writers-${book}`]?.has(window.ship);

    const notesList = notebook?.["notes-by-date"] || [];
    const notes = notebook?.notes || {};
    const showNickname = contact?.nickname && !hideNicknames;

    return (
      <Box
        pt={4}
        mx="auto"
        px={3}
        display="grid"
        gridAutoRows="min-content"
        gridTemplateColumns={["100%", "1fr 1fr"]}
        maxWidth="500px"
        gridRowGap={[4, 6]}
        gridColumnGap={3}
      >
        <Box display={["block", "none"]} gridColumn={["1/2", "1/3"]}>
          <Link to={this.props.rootUrl}>{"<- All Notebooks"}</Link>
        </Box>
        <Box>
          <Text> {notebook?.title}</Text>
          <br />
          <Text color="lightGray">by </Text>
          <Text fontFamily={showNickname ? "sans" : "mono"}>
            {showNickname ? contact?.nickname : ship}
          </Text>
        </Box>
        <Row justifyContent={["flex-start", "flex-end"]}>
          {isWriter && (
            <Link to={relativePath("/new")}>
              <Button primary border style={{ cursor: 'pointer' }}>
                New Post
              </Button>
            </Link>
          )}
          {!isOwn ? (
            this.state.isUnsubscribing ? (
              <Spinner
                awaiting={this.state.isUnsubscribing}
                classes="mt2 ml2"
                text="Unsubscribing..."
              />
            ) : (
              <Button
                ml={isWriter ? 2 : 0}
                destructive
                style={{ cursor: 'pointer' }}
                onClick={() => {
                  this.setState({ isUnsubscribing: true });
                  api.publish
                    .unsubscribeNotebook(deSig(ship), book)
                    .then(() => {
                      history.push(this.props.baseUrl);
                    })
                    .catch(() => {
                      this.setState({ isUnsubscribing: false });
                    });
                }}
              >
                Unsubscribe
              </Button>
            )
          ) : null}
        </Row>
        <Box gridColumn={["1/2", "1/3"]}>
          <Tabs>
            <Tab
              selected={state.tab}
              setSelected={this.setTab}
              label="All Posts"
              id="all"
            />
            <Tab
              selected={state.tab}
              setSelected={this.setTab}
              label="About"
              id="about"
            />
            {isAdmin && (
              <>
                <Tab
                  selected={state.tab}
                  setSelected={this.setTab}
                  label="Subscribers"
                  id="subscribers"
                />
                <Tab
                  selected={state.tab}
                  setSelected={this.setTab}
                  label="Settings"
                  id="settings"
                />
              </>
            )}
          </Tabs>
          {state.tab === "all" && (
            <NotebookPosts
              notes={notes}
              list={notesList}
              host={ship}
              book={book}
              contacts={notebookContacts}
              hideNicknames={hideNicknames}

            />
          )}
          {state.tab === "about" && (
            <Box mt="3" color="black">
              {notebook?.about}
            </Box>
          )}
          {state.tab === "subscribers" && (
            <Subscribers
              host={ship}
              book={book}
              notebook={notebook}
              api={api}
              groups={groups}
            />
          )}
          {state.tab === "settings" && (
            <Settings
              host={ship}
              book={book}
              api={api}
              notebook={notebook}
              contacts={notebookContacts}
              associations={associations}
              groups={groups}
              baseUrl={this.props.baseUrl}
            />
          )}
        </Box>
      </Box>
    );
  }
}
export default Notebook;
