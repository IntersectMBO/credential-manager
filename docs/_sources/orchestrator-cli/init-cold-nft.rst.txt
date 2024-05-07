.. _init_cold_nft:

Initializing the Cold NFT Payment Credential Script
===================================================

The next step is to initialize the payment credential script for the cold
credential NFT.

Prerequisites
-------------

* :ref:`init_cold_committee`

Step 1: Obtaining the X.509 certificate files
---------------------------------------------

To proceed further, you will need to have access to PEM files containing the
certificates in your organization's certificate hierarchy. At the very least,
you will need:

* The self-signed certificate of the root certificate authority
* The signed certificates of the membership and delegation roles

You will need to ask the :ref:`head_of_security` in your organization for these
files. If you are just testing the system out, you can download a folder
containing a fake certificate tree from
`here <https://github.com/IntersectMBO/credential-manager/tree/main/example-certificates>`_.
Either clone the repo locally or use a tool like
`DownGit <https://minhaskamal.github.io/DownGit>`_ to download this folder as a
zip archive.

For the purposes of this guide, we will assume you are using the example
certificates and have saved them to a local folder called
``example-certificates``. ``child-1`` through ``child-3`` will belong to the
membership role, ``child-4`` through ``child-6`` will belong to the delegation
role, and ``child-7`` through ``child-9`` will belong to the voting role.

Step 2: Creating the assets
---------------------------

We can now use ``orchestrator-cli`` to initialize the script:

.. code-block:: bash

   $ orchestrator-cli cold-nft init \
       --testnet \
       --cold-credential-script-file cold-credential/script.plutus \
       --ca-cert example-certificates/ca-cert.pem \
       --membership-cert example-certificates/children/child-1/child-1-cert.pem \
       --membership-cert example-certificates/children/child-2/child-2-cert.pem \
       --membership-cert example-certificates/children/child-3/child-3-cert.pem \
       --delegation-cert example-certificates/children/child-4/child-4-cert.pem \
       --delegation-cert example-certificates/children/child-5/child-5-cert.pem \
       --delegation-cert example-certificates/children/child-6/child-6-cert.pem \
       -o cold-nft


The ``--testnet`` option is used to create the script address, and says that a
testnet address should be created. The alternative is ``--mainnet``. The
``--cold-credential-script-file`` option is used to apply the cold credential
script hash to the script as a parameter. the various ``--*-cert`` options are
used to specify certificates of users to add to the different roles.

Let's see what assets were created.

.. code-block:: bash

   $ ls cold-nft -1
   datum.json
   script.addr
   script.hash
   script.plutus

``datum.json`` contains the initial datum for the script.

.. code-block:: bash

   $ cat cold-nft/datum.json
   {
         "constructor": 0,
         "fields": [
            {
                  "constructor": 0,
                  "fields": [
                     {
                        "bytes": "09159adec41ce5d48dde24a275a5b2c2e79461c8693ef60af9fc3207"
                     },
                     {
                        "bytes": "0ff1fd44947bcd4cdc6f06841d881ac2a0beb3f15ba5f5e3c08991d92e8ba643"
                     }
                  ]
            },
            {
                  "list": [
                     {
                        "constructor": 0,
                        "fields": [
                              {
                                 "bytes": "ff7a6c9f3ebf80ab457cca7813842aa2150d0dad341a7956a334c76d"
                              },
                              {
                                 "bytes": "1a82818b488574c156f1fa8941bad9b4b4976ba21cfaede1ab33a30de39f7edd"
                              }
                        ]
                     },
                     {
                        "constructor": 0,
                        "fields": [
                              {
                                 "bytes": "c2233827cca3a0cc2c49f91a66276c468be994db855d6b413005fa88"
                              },
                              {
                                 "bytes": "3b8536a38eea871cc8b2775deb5861ac4348ef61a84b9e9c643480ae5b88ffc3"
                              }
                        ]
                     },
                     {
                        "constructor": 0,
                        "fields": [
                              {
                                 "bytes": "b23a02a308165c702ce00bf760a0eff33b27b12906e1805b7685125f"
                              },
                              {
                                 "bytes": "fdf913abfdb8f00997cca5c14ca0b82f3d08781015a061e91444425d6f777ffa"
                              }
                        ]
                     }
                  ]
            },
            {
                  "list": [
                     {
                        "constructor": 0,
                        "fields": [
                              {
                                 "bytes": "fc6a114db76d31de585793749dcd6ad2d6c02a52ce9226820656bedd"
                              },
                              {
                                 "bytes": "7c9d1c732c313066ded1568dc24b1230cc782d331cb65465bc65ad5df6fbe832"
                              }
                        ]
                     },
                     {
                        "constructor": 0,
                        "fields": [
                              {
                                 "bytes": "168ff0600f6245812192fb84c1d5a72129ae0445a272acc65dc88fb3"
                              },
                              {
                                 "bytes": "c60e20be4ce0fa457a8c65ade01005475e71880e921c2ee40a6b51d42fd95e11"
                              }
                        ]
                     },
                     {
                        "constructor": 0,
                        "fields": [
                              {
                                 "bytes": "c530a8b72dd72e320e7f4883fcb98d0058e70efcf4e7e0871ce13eb7"
                              },
                              {
                                 "bytes": "ce75748d37a55ef1faec7219708059479197965a5927a7f9901c6bc9707eeaa1"
                              }
                        ]
                     }
                  ]
            }
         ]
      }

We can, and should, sanity check that this datum contains the correct values:

.. code-block:: bash

   $ diff \
      <(cat example-certificates/ca-cert.hash) \
      <(cat cold-nft/datum.json | jq -r '.fields[0].fields[1].bytes')
   $ diff \
      <(cat example-certificates/children/child-1/child-1.keyhash) \
      <(cat cold-nft/datum.json | jq -r '.fields[1].list[0].fields[0].bytes')
   $ diff \
      <(cat example-certificates/children/child-1/child-1-cert.hash) \
      <(cat cold-nft/datum.json | jq -r '.fields[1].list[0].fields[1].bytes')
   $ diff \
      <(cat example-certificates/children/child-2/child-2.keyhash) \
      <(cat cold-nft/datum.json | jq -r '.fields[1].list[1].fields[0].bytes')
   $ diff \
      <(cat example-certificates/children/child-2/child-2-cert.hash) \
      <(cat cold-nft/datum.json | jq -r '.fields[1].list[1].fields[1].bytes')
   $ diff \
      <(cat example-certificates/children/child-3/child-3.keyhash) \
      <(cat cold-nft/datum.json | jq -r '.fields[1].list[2].fields[0].bytes')
   $ diff \
      <(cat example-certificates/children/child-3/child-3-cert.hash) \
      <(cat cold-nft/datum.json | jq -r '.fields[1].list[2].fields[1].bytes')
   $ diff \
      <(cat example-certificates/children/child-4/child-4.keyhash) \
      <(cat cold-nft/datum.json | jq -r '.fields[2].list[0].fields[0].bytes')
   $ diff \
      <(cat example-certificates/children/child-4/child-4-cert.hash) \
      <(cat cold-nft/datum.json | jq -r '.fields[2].list[0].fields[1].bytes')
   $ diff \
      <(cat example-certificates/children/child-5/child-5.keyhash) \
      <(cat cold-nft/datum.json | jq -r '.fields[2].list[1].fields[0].bytes')
   $ diff \
      <(cat example-certificates/children/child-5/child-5-cert.hash) \
      <(cat cold-nft/datum.json | jq -r '.fields[2].list[1].fields[1].bytes')
   $ diff \
      <(cat example-certificates/children/child-6/child-6.keyhash) \
      <(cat cold-nft/datum.json | jq -r '.fields[2].list[2].fields[0].bytes')
   $ diff \
      <(cat example-certificates/children/child-6/child-6-cert.hash) \
      <(cat cold-nft/datum.json | jq -r '.fields[2].list[2].fields[1].bytes')

``script.addr`` contains the script address in bech-32 format:

.. code-block:: bash

   $ cat cold-nft/script.addr
   addr_test1wzq7m97vwjyu43w8snutpnuukhef8nlh7lpx4x53rquzanqmwuk6y

``script.plutus`` contains the compiled script bytes wrapped in a text envelope:

.. code-block:: bash

   $ cat cold-nft/script.plutus
   {
         "type": "PlutusScriptV3",
         "description": "",
         "cborHex": "590fba590fb70101003323232323232323232323232323232323232323232323232323232222259323232329003911480248a4006450029400a450029400a520072222222223232323222222223008014303522590018c00a4432005225900291480ac8a54ccd5cd19b8f0040021333573466e1c00c0060010021801454ca407e4264b26601e00202f18014885640062b26605c0040071593301001f0138999a81c01a00b400e00204b00460008c009180124430021400a264b26601c00202d18014885640062b26605a0040071593300f01f0128999a81b81980ac00e01e048c00118012300248860042800c8c8c8c84c00407cc10c89640063002910ac9982100380146401e46446004032609044b2003180148854ccd5cd19b8f005002159500b898054002442601900209802000913004001096400630028ac99808280200c46005221590018ac99817801480748a40064445001004801e0066466e952000335740609605266ae80dd3982481419aba0374e609400297ae0500a4004456405e3000910c00918012443002026601e52003229004912999ab9a3371e0080042a666ae68cdc78018008c0086000300000722b26601aa00202b18014886400a4444b200518014856400a430028ac8034564cc0e0016404245200322220042264b2660872003222003012c64006444b2005180148856400e3002910ac9981d006480c48a400644440064564cc06c0ac07a2b2043180048860048c009180104600430330018c009221801121801089980600e007864022440028980500380896400a30029109919199ab9a3371266e00cdc1800a400866e18005200430233300823303b22590018c00a442a666ae68cdc78028010c0004c01000400c00500080118108009980f980f0024000446600652003222290079111480248a400a52017229002919820002000ca0044a402e452005280148cc100010004000123223002001303322590018c00244264b2600c0071801801c005100118020009280103232325333573466e1d200000218009919192999ab9a3370e9000001099191919191919191919191919194004cc0908c8c8c94ccd5cd19b8748000008600260606ae8400660626ae84d5d100084c0d12401035054310035573c0046aae74004dd50009aba100f9aba100e998120169aba100d9aba100c9aba100b9981211919192999ab9a3370e90000010c034c0c8d5d0800ccc0cdd69aba13574400215333573466e1d2002002180998191aba100199819bad357426ae880042a666ae68cdc3a40080043003303235742003302f357426ae880042a666ae68cdc3a400c00426500b303335742005303035742003375a6ae84d5d10008d5d10008a999ab9a3370e90040010c024c0c8d5d0800cdd69aba13574400215333573466e1d200a002180a98191aba10010a999ab9a3370e90060010c044c0c8d5d0800cdd69aba13574400215333573466e1d200e00218029bae35742003375c6ae84d5d1000854ccd5cd19b8748040008600e6eb8d5d0800cdd69aba13574400215333573466e1d2012002180098191aba100198191aba13574400215333573466e1d2014002180798191aba10010981a2481035054310035573c0046aae74004dd50009aba100a9aba10099aba1008998123ae3574200f3574200d3574200b35742009357420073574200535742003357426ae880046ae88004d5d10009aba2001357440026ae88004d5d10009aba2001357440026ae88004d5d10009aba2001357440026ae88004d5d1000898112481035054310035573c0046aae74004dd51aba10019919192999ab9a3370e90000010c00cd5d0800854ccd5cd19b87480080086012603c6ae840042a666ae68cdc3a400800430073574200215333573466e1d200600218009aba10019aba13574400215333573466e1d200800218059aba10010a999ab9a3370e90050010c014d5d0800cd5d09aba2001098112481035054310035573c0046aae74004dd51aba1357440021301f491035054310035573c0046aae74004dd50021919192999ab9a3370e90000010c004c070d5d0800854ccd5cd19b874800800860042a666ae68cdc3a4008004300530113574200215333573466e1d200600218030a999ab9a3370e90040010c0204c079241035054310035573c0046aae74004dd50021806002180b802460011301949010350543500180d112c800c6001221801c80148800930040010c0648896400a3000911909800802180f112c800c6006009330070038018024014910ac9980f00100344cc02401401e260080020c0608964006290004884cdc0240046008002301722590018a4001221337009001180200088a400a4445200b222259330110080048ac99805803801c54ca401e2b20071500190c00a4300248564012300290a999ab9a3375e0040022a006300290c00921590048c00a4300290a999ab9a3371e0040022a0063002056401642b2005213301e0020018c00915900190c00a30002300246004088ccc8cc0048c020004cc0048ccd5cd19b874800000600100200a232322322233003002001301b2225900289802800c8856400e26010009221900491194802c8964cc08c01400a2b26601a00800313300c0080068c009159301000489980600400544cc00d64c04000630008c00c01f000001818121112c800c60052219002912c980a000c4cc01c01800e2b26604e012005159330110080018998080061998159112c801440064426600a003003002801800c0071801226600f003004803000c0180c912c800c60012219002912c9804000c4c01800e30020603044b200318004886400a44b2600e003130060038c00800100091919192999ab9a3370e9000001099194004c01cd5d0801cc004d5d08014c004d5d09aba200218038031aba200113012491035054310035573c0046aae74004dd500091919192999ab9a3370e90000010c004dd71aba10019bae357426ae8800426022921035054310035573c0046aae74004dd5000919118011bac00130132233335573e0025000280198021aba100298019aba20024000c04489640063000910c8014894ccd5cd19b87480000044c01800c6004111991914802c8a401a4520092900392999ab9a3371e00400226601c00a006300294009290039400a4a666ae68cdc78010008998070028018c00891400a4500291400a45002911400a450029400a4500291400a500248a401a450029148024a400e4a666ae68cdc78010008998040028018c00a50024a400e500292999ab9a3371e00400226601000a006300224500291400a45002911400a450029400a4500291400a500248a0052280148a401a4500291400a4500291400a4520092900392999ab9a3371e00400226601a00a006300294009290039400a4a666ae68cdc78010008998068028018c008911400a450029400a4500291400a5002488a401e4500291400a4500291400a45002911194803ca40164a666ae68cdc78010008a8018c00a50024a4016500292999ab9a3371e0040022a00630020564cc02001400a26601a00800318012450029400a4500291400a500248a401a4500291400a4500291400a45002911400a4520092900392999ab9a3371e00400226601200a006300294009290039400a4a666ae68cdc78010008998048028018c0089400a4500291400a50024a40164500291400a4500291400a45002911400a45002919805001000c8a005228014a00491480348a0052280148a0052280148a00522280148a005280148a40125200725333573466e3c0080044cc03401400c600528012520072801494ccd5cd19b8f00200113300d00500318011228014a00491480348a0052280148a0052280148a00522280148a005280148a00522900494801c94ccd5cd19b8f00200113300900500318014a00494801ca00525333573466e3c0080044cc02401400c60044a004800888ca400e5200723301400200191400a500248a40125002912999ab9a3371e00800426600a006002300294009290039400a45002919801801000844b200521590029099806001000c6005180122b20032180146001180122b200321801460051800044b200521590029099804001000c60048ac800c8600518000520052280148a0052280148a00522801488a005228014a0052280148a00523300700200111480148a400e4465200b2900492999ab9a3371e0040022a006300294009290049400a4a666ae68cdc78010008a8018c00815900390ac801486400a5200523300c002001911400922290049400a444a666ae68cdc38030018a999ab9a3370e00a00426601e008002300218011180122b20032180146000044666ae68cdc3801000c00200445200529002919806801000ca004948014a00523300d0020010cc0308848896400a2b200318004886004910ac801c600522159330070040028999a980489000803801800c60040480048c8c8c94ccd5cd19b874800000860026eb8d5d0800854c8ccd5cd19b874800800c600a60026ae840082a666ae68cdc3a40080063003375c6ae8400a60026ae84d5d100104c0292401035054310023232325333573466e1d2000002180098059aba10010a999ab9a3370e90010010c00854ccd5cd19b874801000860082601a921035054310035573c0046aae74004dd50009aab9e00235573a0026ea80048c8c8c94ccd5cd19b874800000860026eb8d5d0800cdd69aba13574400213008491035054310035573c0046aae74004dd500091919192999ab9a3370e9000001099194004c8c8c94ccd5cd19b8748000008600260146ae84006660164646464a666ae68cdc3a40000043001300e3574200215333573466e1d20020021328019bad35742005375a6ae840066eb4d5d09aba20011aba2001130104901035054310035573c0046aae74004dd50009aba1357440021300c4901035054310035573c0046aae74004dd51aba100399918009800bad2322300237560026020446666aae7c004a0004650038009bae35573a003300535573c00298021aba20031aba10028001aba10029919192999ab9a3370e90000010c00054ccd5cd19b8748008008600a6eb8d5d0800854ccd5cd19b874801000860066ae84004260189201035054310035573c0046aae74004dd51aba1001998043ae357426ae880046ae88004d5d100089803a481035054310035573c0046aae74004dd500091919192999ab9a3370e90000010c004dd71aba10010a999ab9a3370e90010010c00cdd71aba100109803249035054310035573c0046aae74004dd5000911919192999ab9a3370e90010010c00854ccd5cd19b87480000086002600a6ae840042600c921035054310035573c0046aae74004dd5000919319ab9c00180011480148a400e44a666ae68cdc78020010998038018008c00822333573466e3c008006001002300322590018a5eb8244266ae80c018008c010004600444b200314bd704884cd5d0180280118020008a40064466e9520003357406ea4008cd5d01ba90014bd7008c8c00400488cc00cc008008005300122d87a9f581c7b34b13a4751b7f66f0b3375f48257d4e616103fccd1f8bd0a8aa4edff0001"
      }

``script.hash`` contains a hash of the script bytes as hexadecimal text:

.. code-block:: bash

   $ cat cold-nft/script.hash
   81ed97cc7489cac5c784f8b0cf9cb5f293cff7f7c26a9a9118382ecc

Step 3: Assemble and submit the transaction
-------------------------------------------

Let's make the following assumptions:

1. You have a wallet with ADA in it. The address is saved in a file
   ``orchestrator.addr``.
2. The signing key is in the file ``orchestrator.skey``. In real life, this
   should be on an air-gapped machine.
3. The NFT you are using has a blank token name.
4. You have a local cardano node running and all the correct environment
   variables set.
5. The NFT lives in the output
   ``48823c675bd96a6a3f2d16b57b47afdc11f6af52da8f2e4997ab21b1d40cc43d#0`` along
   with 5000000 lovelace.
6. ``orchestrator.addr`` also has a large amount of ADA in the output
   ``4e09d89d4f538b860195e2dfc4911a4e9c15c4a3b5edc6af51ed76c8413eada4#0``. This
   will be used to cover fees.

Using the assets created in step 2, create a transaction that sends the datum
to the script address:

.. code-block:: bash

   $ cardano-cli conway transaction build \
     --change-address $(cat orchestrator.addr) \
     --tx-in 48823c675bd96a6a3f2d16b57b47afdc11f6af52da8f2e4997ab21b1d40cc43d#0 \
     --tx-in 4e09d89d4f538b860195e2dfc4911a4e9c15c4a3b5edc6af51ed76c8413eada4#0 \
     --tx-out "$(cat cold-nft/script.addr) + 5000000 + 1 $COLD_POLICY_ID" \
     --tx-out-inline-datum-file cold-nft/datum.json \
     --out-file init-cold-nft.body
   Estimated transaction fee: Coin 199557
   $ cardano-cli conway transaction sign \
     --signing-key-file orchestrator.skey \
     --tx-body-file init-cold-nft.body \
     --out-file init-cold-nft.tx
   $ cardano-cli conway transaction submit --tx-file init-cold-nft.tx
   Transaction successfully submitted.

We can query the script address to verify the UTxO is there:

.. code-block:: bash

   $ cardano-cli conway query utxo --address $(cat cold-nft/script.addr) --output-json
   {
      "1b7cea7b907e69fa7963c1f83276295a7d0f36b6460d94922a6f76da16344372#0": {
         "address": "addr_test1wzq7m97vwjyu43w8snutpnuukhef8nlh7lpx4x53rquzanqmwuk6y",
         "datum": null,
         "inlineDatum": {
               "constructor": 0,
               "fields": [
                  {
                     "constructor": 0,
                     "fields": [
                           {
                              "bytes": "09159adec41ce5d48dde24a275a5b2c2e79461c8693ef60af9fc3207"
                           },
                           {
                              "bytes": "0ff1fd44947bcd4cdc6f06841d881ac2a0beb3f15ba5f5e3c08991d92e8ba643"
                           }
                     ]
                  },
                  {
                     "list": [
                           {
                              "constructor": 0,
                              "fields": [
                                 {
                                       "bytes": "ff7a6c9f3ebf80ab457cca7813842aa2150d0dad341a7956a334c76d"
                                 },
                                 {
                                       "bytes": "1a82818b488574c156f1fa8941bad9b4b4976ba21cfaede1ab33a30de39f7edd"
                                 }
                              ]
                           },
                           {
                              "constructor": 0,
                              "fields": [
                                 {
                                       "bytes": "c2233827cca3a0cc2c49f91a66276c468be994db855d6b413005fa88"
                                 },
                                 {
                                       "bytes": "3b8536a38eea871cc8b2775deb5861ac4348ef61a84b9e9c643480ae5b88ffc3"
                                 }
                              ]
                           },
                           {
                              "constructor": 0,
                              "fields": [
                                 {
                                       "bytes": "b23a02a308165c702ce00bf760a0eff33b27b12906e1805b7685125f"
                                 },
                                 {
                                       "bytes": "fdf913abfdb8f00997cca5c14ca0b82f3d08781015a061e91444425d6f777ffa"
                                 }
                              ]
                           }
                     ]
                  },
                  {
                     "list": [
                           {
                              "constructor": 0,
                              "fields": [
                                 {
                                       "bytes": "fc6a114db76d31de585793749dcd6ad2d6c02a52ce9226820656bedd"
                                 },
                                 {
                                       "bytes": "7c9d1c732c313066ded1568dc24b1230cc782d331cb65465bc65ad5df6fbe832"
                                 }
                              ]
                           },
                           {
                              "constructor": 0,
                              "fields": [
                                 {
                                       "bytes": "168ff0600f6245812192fb84c1d5a72129ae0445a272acc65dc88fb3"
                                 },
                                 {
                                       "bytes": "c60e20be4ce0fa457a8c65ade01005475e71880e921c2ee40a6b51d42fd95e11"
                                 }
                              ]
                           },
                           {
                              "constructor": 0,
                              "fields": [
                                 {
                                       "bytes": "c530a8b72dd72e320e7f4883fcb98d0058e70efcf4e7e0871ce13eb7"
                                 },
                                 {
                                       "bytes": "ce75748d37a55ef1faec7219708059479197965a5927a7f9901c6bc9707eeaa1"
                                 }
                              ]
                           }
                     ]
                  }
               ]
         },
         "inlineDatumhash": "7e002022f74e980b99812b2948879ef5ee205c036e9e94a6e8434c1da298b2f1",
         "referenceScript": null,
         "value": {
               "14987a29cf4065e7b38a4cde6bc84b067492ad3ecc8223598a8fe4be": {
                  "": 1
               },
               "lovelace": 5000000
         }
      }
   }
