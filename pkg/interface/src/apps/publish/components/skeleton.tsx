import React, { useMemo } from "react";
import { Box } from "@tlon/indigo-react";
import { Sidebar } from "./lib/Sidebar";
import ErrorBoundary from "../../../components/ErrorBoundary";
import { Notebooks } from "../../../types/publish-update";
import { Path } from "../../../types/noun";
import { SelectedGroup } from "../../../types/local-update";
import { Rolodex } from "../../../types/contact-update";
import { Invites } from "../../../types/invite-update";
import GlobalApi from "../../../api/global";
import { Associations } from "../../../types/metadata-update";
import { RouteComponentProps } from "react-router-dom";

type SkeletonProps = RouteComponentProps<{ ship: string; notebook: string }> & {
  sidebarShown: boolean;
  popout: boolean;
  notebooks: Notebooks;
  active: "sidebar" | "rightPanel";
  invites: Invites;
  associations: Associations;
  selectedGroups: SelectedGroup[];
  contacts: Rolodex;
  api: GlobalApi;
  children: React.ReactNode;
};

export function Skeleton(props: SkeletonProps) {
  const popout = props.popout ? props.popout : false;

  const popoutWindow = popout ? 0 : [0, 3];

  const panelDisplay = props.active === "sidebar" ? ["none", "block"] : "block";

  const path = `${props.match.params.ship}/${props.match.params.notebook}`;

  return (
    <Box height="100%" width="100%" px={popoutWindow} pb={popoutWindow}>
      <Box
        display="flex"
        border={popout ? 0 : 1}
        borderColor="washedGray"
        borderRadius={1}
        width="100%"
        height="100%"
      >
        <Sidebar
          popout={popout}
          sidebarShown={props.sidebarShown}
          active={props.active}
          notebooks={props.notebooks}
          contacts={props.contacts}
          path={path}
          invites={props.invites}
          associations={props.associations}
          selectedGroups={props.selectedGroups}
          api={props.api}
        />
        <Box
          display={panelDisplay}
          width="100%"
          height="100%"
          position="relative"
        >
          <ErrorBoundary>{props.children}</ErrorBoundary>
        </Box>
      </Box>
    </Box>
  );
}

export default Skeleton;
