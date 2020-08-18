import React, { useRef, useEffect } from "react";
import { Route, Switch, useLocation, withRouter } from "react-router-dom";
import { Center, Text } from "@tlon/indigo-react";
import _ from "lodash";

import "./css/custom.css";

import { Skeleton } from "./components/skeleton";
import { NewScreen } from "./components/lib/new";
import { JoinScreen } from "./components/lib/Join";
import { Notebook } from "./components/lib/Notebook";
import { Note } from "./components/lib/Note";
import { NewPost } from "./components/lib/new-post";
import { EditPost } from "./components/lib/edit-post";
import { StoreState } from "../../store/type";
import GlobalApi from "../../api/global";
import GlobalSubscription from "../../subscription/global";
import {NotebookRoutes} from "./components/lib/NotebookRoutes";

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
        "/~publish/notebook/:ship/:notebook/*",
        "/~publish/notebook/:ship/:notebook",
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
            path="/~publish/join/:ship/:notebook"
            render={(props) => {
              const ship = props.match.params.ship || "";
              const notebook = props.match.params.notebook || "";
              return (
                <JoinScreen
                  notebooks={notebooks}
                  ship={ship}
                  book={notebook}
                  api={api}
                  {...props}
                />
              );
            }}
          />
          <Route
            path="/~publish/notebook/:ship/:notebook"
            render={(props) => {
              const view = props.match.params.view
                ? props.match.params.view
                : "posts";

              const popout = Boolean(props.match.params.popout) || false;

              const ship = props.match.params.ship || "";
              const book = props.match.params.notebook || "";

              const bookGroupPath =
                notebooks?.[ship]?.[notebook]?.["subscribers-group-path"];

              const notebookContacts =
                bookGroupPath in contacts ? contacts[bookGroupPath] : {};

              const notebook = notebooks[ship][book];


                return (
                  <NotebookRoutes
                    notebook={notebook}
                    view={view}
                    ship={ship}
                    book={book}
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
            }}
          />
        </Switch>
      </RouterSkeleton>
    </Route>
  );
}
