import React, { useRef, useEffect } from "react";
import { Route, Switch, useLocation, withRouter } from "react-router-dom";
import { Center, Text } from "@tlon/indigo-react";
import _ from "lodash";

import "./css/custom.css";

import { Skeleton } from "./components/skeleton";
import { NewScreen } from "./components/lib/new";
import { JoinScreen } from "./components/lib/join";
import { Notebook } from "./components/lib/notebook";
import { Note } from "./components/lib/note";
import { NewPost } from "./components/lib/new-post";
import { EditPost } from "./components/lib/edit-post";
import { StoreState } from "../../store/type";
import GlobalApi from "../../api/global";
import GlobalSubscription from "../../subscription/global";

type PublishAppProps = StoreState & {
  api: GlobalApi;
  ship: string;
  subscription: GlobalSubscription;
};

const RouterSkeleton = withRouter(Skeleton);

export default function PublishApp(props: PublishAppProps) {
  const unreadTotal = useRef(0);

  useEffect(() => {
    document.title =
      unreadTotal.current > 0
        ? `(${unreadTotal.current}) OS1 - Publish`
        : "OS1 - Publish";
  }, [unreadTotal.current]);

  useEffect(() => {
    props.subscription.startApp("publish");
    props.api.publish.fetchNotebooks();

    return () => {
      props.subscription.stopApp("publish");
    };
  }, []);

  const contacts = props.contacts ? props.contacts : {};
  const selectedGroups = props.selectedGroups ? props.selectedGroups : [];

  const notebooks = props.notebooks ? props.notebooks : {};

  const location = useLocation();

  unreadTotal.current = _.chain(notebooks)
    .values()
    .map(_.values)
    .flatten() // flatten into array of notebooks
    .filter((each) => {
      return (
        selectedGroups
          .map((e) => {
            return e[0];
          })
          .includes(each?.["writers-group-path"]) || selectedGroups.length === 0
      );
    })
    .map("num-unread")
    .reduce((acc, count) => acc + count, 0)
    .value();

  const { api, groups, sidebarShown, invites, associations } = props;

  const active = location.pathname.endsWith("/~publish")
    ? "sidebar"
    : "rightPanel";

  return (
    <Route
      path={[
        "/~publish/notebook/:popout?/:ship/:notebook",
        "/~publish/:popout?/note/:ship/:notebook/:note/:edit?",
        "/~publish",
      ]}
    >
      <RouterSkeleton
        popout={location.pathname.includes("popout/")}
        active={active}
        sidebarShown={sidebarShown}
        invites={invites}
        notebooks={notebooks}
        associations={associations}
        selectedGroups={selectedGroups}
        contacts={contacts}
        api={api}
      >
        <Switch>
          <Route exact path="/~publish">
            <Center width="100%" height="100%">
              <Text color="lightGray">
                Select or create a notebook to begin.
              </Text>
            </Center>
          </Route>
          <Route
            exact
            path="/~publish/new"
            render={(props) => {
              return (
                <NewScreen
                  associations={associations}
                  api={api}
                  notebooks={notebooks}
                  {...props}
                />
              );
            }}
          />
          <Route
            exact
            path="/~publish/join/:ship?/:notebook?"
            render={(props) => {
              const ship = props.match.params.ship || "";
              const notebook = props.match.params.notebook || "";
              return (
                <JoinScreen
                  notebooks={notebooks}
                  ship={ship}
                  notebook={notebook}
                  api={api}
                  {...props}
                />
              );
            }}
          />
          <Route
            exact
            path="/~publish/:popout?/notebook/:ship/:notebook/:view?"
            render={(props) => {
              const view = props.match.params.view
                ? props.match.params.view
                : "posts";

              const popout = Boolean(props.match.params.popout) || false;

              const ship = props.match.params.ship || "";
              const notebook = props.match.params.notebook || "";

              const bookGroupPath =
                notebooks?.[ship]?.[notebook]?.["subscribers-group-path"];

              const notebookContacts =
                bookGroupPath in contacts ? contacts[bookGroupPath] : {};

              if (view === "new") {
                return (
                  <NewPost
                    notebooks={notebooks}
                    ship={ship}
                    book={notebook}
                    sidebarShown={sidebarShown}
                    popout={popout}
                    api={api}
                    {...props}
                  />
                );
              } else {
                return (
                  <Notebook
                    notebooks={notebooks}
                    view={view}
                    ship={ship}
                    book={notebook}
                    groups={groups}
                    contacts={contacts}
                    notebookContacts={notebookContacts}
                    associations={associations.contacts}
                    sidebarShown={sidebarShown}
                    popout={popout}
                    api={api}
                    {...props}
                  />
                );
              }
            }}
          />
          <Route
            exact
            path="/~publish/:popout?/note/:ship/:notebook/:note/:edit?"
            render={(props) => {
              const ship = props.match.params.ship || "";
              const notebook = props.match.params.notebook || "";
              const note = props.match.params.note || "";

              const popout = Boolean(props.match.params.popout) || false;

              const bookGroupPath =
                notebooks?.[ship]?.[notebook]?.["subscribers-group-path"];
              const notebookContacts =
                bookGroupPath in contacts ? contacts[bookGroupPath] : {};

              const edit = Boolean(props.match.params.edit) || false;

              if (edit) {
                return (
                  <EditPost
                    notebooks={notebooks}
                    book={notebook}
                    note={note}
                    ship={ship}
                    sidebarShown={sidebarShown}
                    popout={popout}
                    api={api}
                    {...props}
                  />
                );
              } else {
                return (
                  <Note
                    notebooks={notebooks}
                    book={notebook}
                    groups={groups}
                    contacts={notebookContacts}
                    ship={ship}
                    note={note}
                    sidebarShown={sidebarShown}
                    popout={popout}
                    api={api}
                    {...props}
                  />
                );
              }
            }}
          />
        </Switch>
      </RouterSkeleton>
    </Route>
  );
}
