import React from 'react';

import { MetadataColor } from './color';
import { MetadataInput } from './input';
import { Box } from '@tlon/indigo-react';
import { uxToHex } from '~/logic/lib/util';


export const MetadataSettings = (props) => {
  const {
    isOwner,
    association,
    changeLoading,
    api,
    resource,
    app,
    module
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
    <Box mt='6'>
      <MetadataInput
        title='Rename'
        description={`Change the name of this ${resource}`}
        isDisabled={!isOwner}
        initialValue={title}
        setValue={(val) => {
          changeLoading(false, true, `Editing ${resource}...`, () => {
            api.metadata.metadataAdd(
              app,
              association['app-path'],
              association['group-path'],
              val,
              association.metadata.description,
              association.metadata['date-created'],
              uxToHex(association.metadata.color),
              module
            ).then(() => {
              changeLoading(false, false, '', () => {});
            });
          });
        }} />
        <MetadataInput
          title='Change description'
          description={`Change the description of this ${resource}`}
          isDisabled={!isOwner}
          initialValue={description}
          setValue={(val) => {
            changeLoading(false, true, `Editing ${resource}...`, () => {
              api.metadata.metadataAdd(
                app,
                association['app-path'],
                association['group-path'],
                association.metadata.title,
                val,
                association.metadata['date-created'],
                uxToHex(association.metadata.color),
                module
              ).then(() => {
                changeLoading(false, false, '', () => {});
              });
            });
          }} />
      <MetadataColor
        initialValue={color}
        isDisabled={!isOwner}
        resource={resource}
        setValue={(val) => {
          changeLoading(false, true, `Editing ${resource}...`, () => {
            props.api.metadata.metadataAdd(
              app,
              association['app-path'],
              association['group-path'],
              association.metadata.title,
              association.metadata.description,
              association.metadata['date-created'],
              val,
              module
            ).then(() => {
              changeLoading(false, false, '', () => {});
            });
          });
        }} />
    </Box>
  );
};

