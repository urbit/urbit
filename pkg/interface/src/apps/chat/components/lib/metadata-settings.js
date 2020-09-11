import React, { Component } from 'react';

import { MetadataColor } from './metadata-color';
import { MetadataInput } from './metadata-input';
import { uxToHex } from '../../../../lib/util';


export const MetadataSettings = (props) => {
  const {
    isOwner,
    association,
    changeLoading,
    api,
    station
  } = props;

  const title = 
    (props.association && 'metadata' in props.association) ?
    association.metadata.title : '';
  const description = 
    (props.association && 'metadata' in props.association) ?
    association.metadata.description : '';
  const color =
    (props.association && 'metadata' in props.association) ?
    `#${uxToHex(props.association.metadata.color)}` : '';

  return (
    <div className="cf mt6">
      <MetadataInput
        title='Rename'
        description='Change the name of this chat'
        isDisabled={!isOwner}
        initialValue={title}
        setValue={(val) => {
          changeLoading(false, true, 'Editing chat...', () => {
            api.metadata.metadataAdd(
              'chat',
              association['app-path'],
              association['group-path'],
              val,
              association.metadata.description,
              association.metadata['date-created'],
              uxToHex(association.metadata.color)
            ).then(() => {
              changeLoading(false, false, '', () => {});
            });
          });
        }} />
        <MetadataInput
          title='Change description'
          description='Change the description of this chat'
          isDisabled={!isOwner}
          initialValue={description}
          setValue={(val) => {
            changeLoading(false, true, 'Editing chat...', () => {
              api.metadata.metadataAdd(
                'chat',
                association['app-path'],
                association['group-path'],
                association.metadata.title,
                val,
                association.metadata['date-created'],
                uxToHex(association.metadata.color)
              ).then(() => {
                changeLoading(false, false, '', () => {});
              });
            });
          }} />
      <MetadataColor
        initialValue={color} 
        isDisabled={!isOwner}
        setValue={(val) => {
          changeLoading(false, true, 'Editing chat...', () => {
            props.api.metadata.metadataAdd(
              'chat',
              association['app-path'],
              association['group-path'],
              association.metadata.title,
              association.metadata.description,
              association.metadata['date-created'],
              val
            ).then(() => {
              changeLoading(false, false, '', () => {});
            });
          });
        }} />
    </div>
  );
};

