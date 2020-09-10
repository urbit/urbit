import React, { Component } from "react";
import { Box, Text } from "@tlon/indigo-react";
import { NotebookItem } from "./NotebookItem";
import { Association, AppAssociations } from "~/types";

interface GroupItemProps {
  groupAssociation: Association;
  appAssociations: AppAssociations;
  appPaths: string[];
  path?: string;
}

export class GroupItem extends Component<GroupItemProps> {
  render() {
    const { props } = this;

    const title =
      props.groupAssociation?.metadata?.title || "Unmanaged Notebooks";
    return (
      <Box>
        <Box fontSize={0} px={3} fontWeight="700" pb={2} color="lightGray">
          {title}
        </Box>
        {props.appPaths.map((appPath, i) => {
          const { metadata } = props.appAssociations?.[appPath] || {};
          return (
            <NotebookItem
              key={i}
              unreadCount={0}
              title={metadata?.title}
              path={appPath}
              selected={props.path === appPath}
            />
          );
        })}
      </Box>
    );
  }
}

export default GroupItem;
