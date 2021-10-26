import { Association, GraphConfig, Group } from '@urbit/api';
import _ from 'lodash';
import React from 'react';
import { Redirect, Route, Switch } from 'react-router-dom';

export function getGraphPermalink(
  assoc: Association,
  group: Group,
  index: string
) {
  const mod = (assoc.metadata.config as GraphConfig).graph;
  const groupPath = group.hidden
    ? '/~landscape/home'
    : `/~landscape${assoc.group}`;
  if (mod === 'chat') {
    return getChatPermalink(
      group.hidden ? '/~landscape/messages' : `/~landscape${assoc.group}`,
      assoc,
      index
    );
  } else if (mod === 'publish') {
    return getPublishPermalink(groupPath, assoc, index);
  } else if (mod === 'link') {
    return getLinkPermalink(groupPath, assoc, index);
  } else if (mod === 'post') {
    return getPostPermalink(groupPath, assoc, index);
  }
  return '/~404';
}

function getPostPermalink(
  groupPath: string,
  assoc: Association,
  index: string
) {
  const base = `${groupPath}/feed/thread`;
  return base + index;
}

function getPublishPermalink(
  groupPath: string,
  assoc: Association,
  index: string
) {
  const idx = index.split('/').slice(1);
  const base = `${groupPath}/resource/publish${assoc.resource}`;
  let isComment = false;
  const res = _.reduce(
    idx,
    (acc, val, i) => {
      if (i === 0) {
        return { ...acc, pathname: `${acc.pathname}/note/${val}` };
      } else if (i === 1 && val === '2') {
        isComment = true;
        return acc;
      } else if (i === 2 && isComment) {
        return { ...acc, search: `?selected=${val}` };
      }
      return acc;
    },
    { pathname: base }
  );
  return res;
}

function getLinkPermalink(
  groupPath: string,
  assoc: Association,
  index: string
) {
  const idx = index.split('/').slice(1);
  const base = `${groupPath}/resource/link${assoc.resource}`;
  const res = _.reduce(
    idx,
    (acc, val, i) => {
      if (i === 0) {
        return { ...acc, pathname: `${acc.pathname}/index/${val}` };
      } else if (i === 1) {
        return { ...acc, search: `?selected=${val}` };
      }
      return acc;
    },
    { pathname: base }
  );
  return res;
}

function getChatPermalink(
  groupPath: string,
  assoc: Association,
  index: string
) {
  const idx = index.split('/').slice(1);
  if (idx.length === 0) {
    return `${groupPath}/resource/chat${assoc.resource}`;
  }
  return `${groupPath}/resource/chat${assoc.resource}?msg=${idx[0]}`;
}

export function GraphIndexRoute(props: {
  association: Association;
  group: Group;
  index: string;
  url: string;
}) {
  const { url, index, association, group } = props;

  return (
    <Switch>
      <Route
        path={`${url}/:id`}
        render={({ match }) => {
          const newUrl = `${url}/${match.params.id}`;
          const newIndex = `${index}/${match.params.id}`;
          return (
            <GraphIndexRoute
              group={group}
              url={newUrl}
              association={association}
              index={newIndex}
            />
          );
        }}
      />
      <Route path="">
        <Redirect
          to={getGraphPermalink(association, group, index)}
        />
      </Route>
    </Switch>
  );
}
