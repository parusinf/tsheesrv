create or replace function F_PSTS_CONFIRMED
(
  nCOMPANY                  in number,       -- �����������
  nRN                       in number,       -- RN ������ (������)
  dWORKDATE                 in date          -- ������ ������
)
return number                                -- 0: ������ �� ���������, 1: ������ ���������
as
  nALL_CONF                 number(1);
begin
  select case when CMP_NUM(count(C.PSORGGRP),sum(PR.TAB_CONF))=0 or count(C.PSORGGRP)=0 then 0 else 1 end
    into nALL_CONF
    from PSPAYCARD    C,
         PSPAYCARDPRD PR
   where C.RN         = PR.PRN (+)
     and(PR.YEAR  (+) = extract(year  from dWORKDATE)
     and PR.MONTH (+) = extract(month from dWORKDATE))
     and C.DATE_FROM <= last_day(dWORKDATE)
     and(C.DATE_TO   >= trunc(dWORKDATE) or C.DATE_TO is null)
     and C.PSORGGRP   = nRN
     and C.COMPANY    = nCOMPANY;
  return nALL_CONF;
end;
/
show errors;

create or replace function F_PSTS_SAL_SHEET
(
  nCOMPANY        in number,            -- �����������
  nRN             in number,            -- RN ������ (������)
  dWORKDATE       in date               -- ������ ������
)
return number                                -- 0: ��������� �� ������������, 1: ��������� ������������
as
  nSAL_SHEET      number;
begin
  select case when exists
           ( select /*+ INDEX(L I_DOCLINKS_IN_DOCUMENT) */ null
               from DOCLINKS L,
                    PSPAYCARD    C,
                    PSSHEET       S
              where L.IN_DOCUMENT  = C.RN
                and L.IN_UNITCODE  = 'ParentPayCards'
                and L.OUT_UNITCODE = 'ParentSalarySheets'
                and C.PSORGGRP = nRN
                and C.COMPANY = nCOMPANY
                and L.OUT_DOCUMENT = S.RN
                and extract(year  from dWORKDATE) = S.YEAR
                and extract(month from dWORKDATE) = S.MONTH
                and C.DATE_FROM <= last_day(dWORKDATE)
                and(C.DATE_TO  >= trunc(dWORKDATE) or C.DATE_TO is null) ) then 1
         else 0 end
    into nSAL_SHEET
    from dual;
  return nSAL_SHEET;
end;
/
show errors;

create or replace trigger T_PSPAYCARDHOUR_BUPDATE
  before update on PSPAYCARDHOUR for each row
declare
  nPSORGGRP       PKG_STD.tREF;
  sORGCODE        PSORG.CODE%type;
  sGRCODE         PSORGGRP.CODE%type;
  dWORKDATE       date;
begin
  /* �������� ������������ �������� ����� */
  PKG_UNCHANGE.CHECK_NE('PSPAYCARDHOUR', 'RN', :new.RN, :old.RN);
  PKG_UNCHANGE.CHECK_NE('PSPAYCARDHOUR', 'COMPANY', :new.COMPANY, :old.COMPANY);
  PKG_UNCHANGE.CHECK_NE('PSPAYCARDHOUR', 'PRN', :new.PRN, :old.PRN);

  if CMP_NUM(:new.WORKEDHOURS, :old.WORKEDHOURS) = 0 then
    select C.PSORGGRP,
         O.CODE   OCODE,
         OG.CODE  OGCODE,
         CD.WORKDATE
    into nPSORGGRP,
         sORGCODE,
         sGRCODE,
         dWORKDATE
    from PSPAYCARD    C,
         PSPAYCARDDAY CD,
         PSORG        O,
         PSORGGRP     OG
   where C.RN       = CD.PRN
     and CD.RN      = :old.PRN
     and C.PSORG    = O.RN
     and C.PSORGGRP = OG.RN;

    p_exception(abs(F_PSTS_CONFIRMED(:new.COMPANY, nPSORGGRP, dWORKDATE)-1),
      '���������� ��������� ���� � ��������� �������� �� ������ "'||sGRCODE||'" ����������� "'||sORGCODE||'", �.�. �� ���� ������ ��� �������� ������.' );
    p_exception(abs(F_PSTS_SAL_SHEET(:new.COMPANY, nPSORGGRP, dWORKDATE)-1),
      '���������� ��������� ���� � ��������� �������� �� ������ "'||sGRCODE||'" ����������� "'||sORGCODE||'", �.�. �� ���� ������ ������������ ���������.' );
  end if;

  /* ��� ��������� ���������� ��������� ��������� ������� �� ������������ */
  if (CMP_NUM(:old.CRN,:new.CRN) = 0) then
    return;
  end if;

  /* ����������� ������� */
  if ( PKG_IUD.PROLOGUE('PSPAYCARDHOUR', 'U') ) then
    PKG_IUD.REG_RN('RN', :new.RN, :old.RN);
    PKG_IUD.REG_COMPANY('COMPANY', :new.COMPANY, :old.COMPANY);
    PKG_IUD.REG_CRN('CRN', :new.CRN, :old.CRN);
    PKG_IUD.REG_PRN('PRN', :new.PRN, :old.PRN);
    PKG_IUD.REG(1, 'HOURSTYPE', :new.HOURSTYPE, :old.HOURSTYPE);
    PKG_IUD.REG('WORKEDHOURS', :new.WORKEDHOURS, :old.WORKEDHOURS);
    PKG_IUD.EPILOGUE;
  end if;
end;
/
show errors;

create or replace trigger T_PSPAYCARDDAY_BUPDATE
  before update on PSPAYCARDDAY for each row
declare
  nPSORGGRP                              PKG_STD.tREF;
  sORGCODE                               PSORG.CODE%type;
  sGRCODE                                PSORGGRP.CODE%type;
begin
  /* �������� ������������ �������� ����� */
  PKG_UNCHANGE.CHECK_NE('PSPAYCARDDAY', 'RN', :new.RN, :old.RN);
  PKG_UNCHANGE.CHECK_NE('PSPAYCARDDAY', 'COMPANY', :new.COMPANY, :old.COMPANY);
  PKG_UNCHANGE.CHECK_NE('PSPAYCARDDAY', 'PRN', :new.PRN, :old.PRN);

  /* ��� ��������� ���������� ��������� ��������� ������� �� ������������ */
  if (CMP_NUM(:old.CRN,:new.CRN) = 0) then
    return;
  end if;

  if CMP_NUM(:new.DAYSTYPE, :old.DAYSTYPE) = 0 then
    select C.PSORGGRP,
           O.CODE   OCODE,
           OG.CODE  OGCODE
      into nPSORGGRP,
           sORGCODE,
           sGRCODE
      from PSPAYCARD    C,
           PSORG        O,
           PSORGGRP     OG
     where C.RN      = :old.PRN
       and C.PSORG    = O.RN
       and C.PSORGGRP = OG.RN;

    p_exception(abs(F_PSTS_CONFIRMED(:new.COMPANY, nPSORGGRP, :new.WORKDATE)-1),
      '���������� ��������� ��� ��� � ��������� �������� �� ������ "'||sGRCODE||'" ����������� "'||sORGCODE||'", �.�. �� ���� ������ ��� �������� ������.' );
    p_exception(abs(F_PSTS_SAL_SHEET(:new.COMPANY, nPSORGGRP, :new.WORKDATE)-1),
      '���������� ��������� ��� ��� � ��������� �������� �� ������ "'||sGRCODE||'" ����������� "'||sORGCODE||'", �.�. �� ���� ������ ������������ ���������.' );
  end if;

  /* ����������� ������� */
  if ( PKG_IUD.PROLOGUE('PSPAYCARDDAY', 'U') ) then
    PKG_IUD.REG_RN('RN', :new.RN, :old.RN);
    PKG_IUD.REG_COMPANY('COMPANY', :new.COMPANY, :old.COMPANY);
    PKG_IUD.REG_CRN('CRN', :new.CRN, :old.CRN);
    PKG_IUD.REG_PRN('PRN', :new.PRN, :old.PRN);
    PKG_IUD.REG(1, 'WORKDATE', :new.WORKDATE, :old.WORKDATE);
    PKG_IUD.REG('DAYSTYPE', :new.DAYSTYPE, :old.DAYSTYPE);
    PKG_IUD.EPILOGUE;
  end if;
end;
/
show errors;

create or replace view V_PSTSGRP
(
  nRN,                                  -- ��������������� �����
  nCOMPANY,                             -- �����������
  nCRN,                                 -- �������
  nPRN,                                 -- ��������
  sORG_CODE,                            -- �������� ����������
  sORG_NAME,                            -- ������������ ����������
  sCODE,                                -- ��������
  sNAME,                                -- ������������
  nGROUPKND,                            -- ��������� ������
  sGROUPKND,                            -- �������� ��������� ������
  nSHEDULE,                             -- ������ ������
  sSHEDULE,                             -- �������� ������� ������
  nPRDFORM,                             -- ����� ��������
  sPRDFORM,                             -- �������� ����� ��������
  dDATE_FROM,                           -- ��������� �
  dDATE_TO,                             -- ��������� ��
  nTAB_CONF,                            -- ������ ���������
  nIS_SAL_SHEET                         -- ���������
)
as
select
  T.RN,                                 -- nRN
  T.COMPANY,                            -- nCOMPANY
  T.CRN,                                -- nCRN
  T.PRN,                                -- nPRN
  M.CODE,                               -- sORG_CODE
  M.NAME,                               -- sORG_NAME
  T.CODE,                               -- sCODE
  T.NAME,                               -- sNAME
  T.GROUPKND,                           -- nGROUPKND
  P.CODE,                               -- sGROUPKND
  T.SHEDULE,                            -- nSHEDULE
  S.CODE,                               -- sSHEDULE
  T.PRDFORM,                            -- nPRDFORM
  P2.CODE,                              -- sPRDFORM
  T.DATE_FROM,                          -- dDATE_FROM
  T.DATE_TO,                            -- dDATE_TO
  F_PSTS_CONFIRMED(T.COMPANY, T.RN, GET_OPTIONS_DATE('ParentPayCards_CalcPeriod', T.COMPANY)),    -- nTAB_CONF
  F_PSTS_SAL_SHEET(T.COMPANY, T.RN, GET_OPTIONS_DATE('ParentPayCards_CalcPeriod', T.COMPANY))     -- nIS_SAL_SHEET
from
  PSORGGRP T,
  PSORG M,
  PSGRPKND P,
  SLSCHEDULE S,
  PREDFORM P2
where T.PRN = M.RN
  and T.GROUPKND = P.RN
  and T.SHEDULE = S.RN
  and T.PRDFORM = P2.RN(+)
  and exists (select null from V_USERPRIV UP where UP.CATALOG = T.CRN)
;

create or replace procedure P_PSTS_FOVDISTRIBH
(
  nCOMPANY                  in number,       -- �����������
  nIDENT                    in number,
  dBGN                      in date,
  dEND                      in date,
  sHOURSTYPE                in varchar2,
  nOUTDAYOFF                in number,
  nACCOUNT                  in number,
  nINSERT                   in number
)
as
begin
  for rREC in
  (
    select
      C.RN,
      C.CRN,
      G.CODE  GRCODE,
      OG.CODE GCODE,
      L.DOCUMENT
    from
      SELECTLIST L,
      PSORGGRP   G,
      PSORG      OG,
      PSPAYCARD  C
    where L.IDENT = nIDENT
      and G.RN = L.DOCUMENT
      and C.PSORGGRP = G.RN
      and C.DATE_FROM <= dEND
      and (C.DATE_TO >= dBGN or C.DATE_TO is null)
      and OG.RN = G.PRN
    order by G.RN
  )
  loop
    p_exception(abs(F_PSTS_CONFIRMED(nCOMPANY, rREC.DOCUMENT, dBGN)-1),
      '���������� �������� ���� ������ �� ������ "'||rREC.GRCODE||'" ����������� "'||rREC.GCODE||'", �.�. ���� ������ ��� ��������.' );
    p_exception(abs(F_PSTS_SAL_SHEET(nCOMPANY, rREC.DOCUMENT, dBGN)-1),
      '���������� �������� ���� ������ �� ������ "'||rREC.GRCODE||'" ����������� "'||rREC.GCODE||'", �.�. ���� �� ����� ������ ���� �������������� ���������.' );
    /* �������� ������ ���������� �������� */
    PKG_ENV.PROLOGUE( nCOMPANY, null, rREC.CRN, 'ParentPayCards', 'PSPAYCARD_FOVDISTRIBH', 'PSPAYCARD', rREC.RN );

    PKG_PSPAYCARDTIME.FOV_DISTRIBH( nCOMPANY, rREC.RN, dBGN, dEND, sHOURSTYPE, nOUTDAYOFF, nACCOUNT, nINSERT );

    /* �������� ��������� ���������� �������� */
    PKG_ENV.EPILOGUE( nCOMPANY, null, rREC.CRN, 'ParentPayCards', 'PSPAYCARD_FOVDISTRIBH', 'PSPAYCARD', rREC.RN );
  end loop;
end;
/
show errors;

create or replace procedure P_PSTS_FOVDISTRIBD
(
  nCOMPANY      in number,      -- �����������
  nIDENT        in number,      -- ������������� ���������� �������
  dBGN          in date,        -- ������ �
  dEND          in date,        -- ������ ��
  sDAYTYPE      in varchar2,    -- �������� ���� ���
  nOUTDAYOFF    in number       -- ��������� ��������� ��� �� ������� (0 - ���, 1 - ��)
)
as
begin
  for rREC in
  (
    select
      C.RN,
      C.CRN,
      G.CODE  GRCODE,
      OG.CODE GCODE,
      L.DOCUMENT
    from
      SELECTLIST L,
      PSORGGRP   G,
      PSORG      OG,
      PSPAYCARD  C
    where L.IDENT = nIDENT
      and G.RN = L.DOCUMENT
      and C.PSORGGRP = G.RN
      and C.DATE_FROM <= dEND
      and (C.DATE_TO >= dBGN or C.DATE_TO is null)
      and OG.RN = G.PRN
    order by G.RN
  )
  loop
    p_exception(abs(F_PSTS_CONFIRMED(nCOMPANY, rREC.DOCUMENT, dBGN)-1),
      '���������� �������� ��� ������ �� ������ "'||rREC.GRCODE||'" ����������� "'||rREC.GCODE||'", �.�. ���� ������ ��� ��������.' );
    p_exception(abs(F_PSTS_SAL_SHEET(nCOMPANY, rREC.DOCUMENT, dBGN)-1),
      '���������� �������� ��� ������ �� ������ "'||rREC.GRCODE||'" ����������� "'||rREC.GCODE||'", �.�. ���� �� ����� ������ ���� �������������� ���������.' );
    /* �������� ������ ���������� �������� */
    PKG_ENV.PROLOGUE( nCOMPANY, null, rREC.CRN, 'ParentPayCards', 'PSPAYCARD_FOVDISTRIBD', 'PSPAYCARD', rREC.RN );

    PKG_PSPAYCARDTIME.FOV_DISTRIBD( nCOMPANY, rREC.RN, dBGN, dEND, sDAYTYPE, nOUTDAYOFF );

    /* �������� ��������� ���������� �������� */
    PKG_ENV.EPILOGUE( nCOMPANY, null, rREC.CRN, 'ParentPayCards', 'PSPAYCARD_FOVDISTRIBD', 'PSPAYCARD', rREC.RN );
  end loop;
end;
/
show errors;

create or replace procedure P_PSTS_FOVCREATE
(
  nCOMPANY                  in number,       -- �����������
  nIDENT                    in number,
  nCALCYEAR                 in number,
  nCALCMONTH                in number
)
as
  dBGN                      date;
  dEND                      date;
begin
  dBGN := INT2DATE( 1, nCALCMONTH, nCALCYEAR );
  dEND := last_day( dBGN );

  for rREC in
  (
    select
      C.RN,
      C.CRN,
      G.CODE  GRCODE,
      OG.CODE GCODE,
      L.DOCUMENT
    from
      SELECTLIST L,
      PSORGGRP   G,
      PSORG      OG,
      PSPAYCARD  C
    where L.IDENT = nIDENT
      and G.RN = L.DOCUMENT
      and C.PSORGGRP = G.RN
      and C.DATE_FROM <= dEND
      and (C.DATE_TO >= dBGN or C.DATE_TO is null)
      and OG.RN = G.PRN
    order by G.RN
  )
  loop
    p_exception(abs(F_PSTS_CONFIRMED(nCOMPANY, rREC.DOCUMENT, dBGN)-1),
      '���������� ������������ ������ �� ��������� �� ������ "'||rREC.GRCODE||'" ����������� "'||rREC.GCODE||'", �.�. ���� ������ ��� ��������.' );
    p_exception(abs(F_PSTS_SAL_SHEET(nCOMPANY, rREC.DOCUMENT, dBGN)-1),
      '���������� ������������ ������ �� ��������� �� ������ "'||rREC.GRCODE||'" ����������� "'||rREC.GCODE||'", �.�. ���� �� ����� ������ ���� �������������� ���������.' );
    /* �������� ������ ���������� �������� */
    PKG_ENV.PROLOGUE( nCOMPANY, null, rREC.CRN, 'ParentPayCards', 'PSPAYCARD_FOVCREATE', 'PSPAYCARD', rREC.RN );

    PKG_PSPAYCARDTIME.FOV_CREATE( nCOMPANY, rREC.RN, dBGN, dEND );

    /* �������� ��������� ���������� �������� */
    PKG_ENV.EPILOGUE( nCOMPANY, null, rREC.CRN, 'ParentPayCards', 'PSPAYCARD_FOVCREATE', 'PSPAYCARD', rREC.RN );
  end loop;
end;
/
show errors;

create or replace procedure P_PSPAYCARD_PSTS_UNCONFIRM
(
  nCOMPANY                  in number,
  nIDENT                    in number,       -- ���������� ������
  nUNCONFIRMED             out number        -- ������� ����������
)
as
  dCALCPER                     date          :=GET_OPTIONS_DATE('ParentPayCards_CalcPeriod', nCOMPANY);
  nALL_CONF                    number;
  nOLD_CRN                     number        :=null;
  nOLD_C_RN                    number        :=null;
begin
  nUNCONFIRMED:= 0;
  -- ���� �� �������
  for rRECC in
  (
     select SL.DOCUMENT,
            G.CODE        GCODE,
            GR.CODE       GRCODE
       from SELECTLIST    SL,
            PSORGGRP      GR,
            PSORG         G
      where SL.IDENT    = nIDENT
        and SL.DOCUMENT = GR.RN
        and G.RN        = GR.PRN
  )
  loop
    p_exception(F_PSTS_CONFIRMED(nCOMPANY, rRECC.DOCUMENT, dCALCPER),
      '���������� ����� ����������� � ������ �� ������ "'||rRECC.GRCODE||'" ����������� "'||rRECC.GCODE||'", �.�. ���� ������ ��� �� ��������.' );
    p_exception(abs(F_PSTS_SAL_SHEET(nCOMPANY, rRECC.DOCUMENT, dCALCPER)-1),
      '���������� ����� ����������� � ������ �� ������ "'||rRECC.GRCODE||'" ����������� "'||rRECC.GCODE||'", �.�. ���� �� ����� ������ ���� �������������� ���������.' );
    -- ���� �� ��
    for rREC in
    (
       select C.RN           C_RN,
              C.CRN,
              PR.RN          PR_RN,
              C.PSORGGRP
        from  PSPAYCARD      C,
              PSPAYCARDPRD   PR
        where C.PSORGGRP   = rRECC.DOCUMENT
          and C.RN         = PR.PRN
          and PR.TAB_CONF  = 1
          and C.DATE_FROM <= last_day(dCALCPER)
          and(C.DATE_TO   >= trunc(dCALCPER, 'MM') or C.DATE_TO is null)
          and PR.YEAR      = extract(year from dCALCPER)
          and PR.MONTH     = extract(month from dCALCPER)
    )
    loop
      if CMP_NUM(rREC.C_RN, nOLD_C_RN)=0 then
        if nOLD_C_RN is not null then
          -- �������� ��������� ���������� ��������
          PKG_ENV.EPILOGUE( nCOMPANY, null, nOLD_CRN, 'ParentPayCards', 'PSPAYCARD_PSTS_UNCONFIRM', 'PSPAYCARD', nOLD_C_RN );
        end if;
        -- �������� ������ ���������� ��������
        PKG_ENV.PROLOGUE( nCOMPANY, null, rREC.CRN, 'ParentPayCards', 'PSPAYCARD_PSTS_UNCONFIRM', 'PSPAYCARD', rREC.C_RN );
      end if;

      update PSPAYCARDPRD PR
         set TAB_CONF = 0
       where PR.RN = rREC.PR_RN;
      nUNCONFIRMED:= nUNCONFIRMED+1;

      nOLD_CRN:=  rREC.CRN;
      nOLD_C_RN:=  rREC.C_RN;
    end loop;
    if nOLD_C_RN is not null then
      -- �������� ��������� ���������� ��������
      PKG_ENV.EPILOGUE( nCOMPANY, null, nOLD_CRN, 'ParentPayCards', 'PSPAYCARD_PSTS_UNCONFIRM', 'PSPAYCARD', nOLD_C_RN );
    end if;
  end loop;
end;
/
show errors;

create or replace procedure P_PSPAYCARD_PSTS_CONFIRM
(
  nCOMPANY                  in number,
  nIDENT                    in number,      -- ���������� ������
  nCONFIRMED               out number       -- ������� ����������
)
as
  dCALCPER                     date         := GET_OPTIONS_DATE('ParentPayCards_CalcPeriod', nCOMPANY);
  nOLD_CRN                     number       :=null;
  nOLD_C_RN                    number       :=null;
begin
  nCONFIRMED:= 0;
  -- ���� �� �������
  for rRECC in
  (
     select SL.DOCUMENT,
            G.CODE        GCODE,
            GR.CODE       GRCODE
       from SELECTLIST    SL,
            PSORGGRP      GR,
            PSORG         G
      where SL.IDENT    = nIDENT
        and SL.DOCUMENT = GR.RN
        and G.RN        = GR.PRN
  )
  loop
    p_exception(abs(F_PSTS_CONFIRMED(nCOMPANY, rRECC.DOCUMENT, dCALCPER)-1),
      '���������� ��������� ������ �� ������ "'||rRECC.GRCODE||'" ����������� "'||rRECC.GCODE||'", �.�. ���� ������ ��� ��������.' );
    -- ���� �� ��
    for rREC in
    (
       select C.RN    C_RN,
              C.CRN,
              PR.RN   PR_RN,
              PR.TAB_CONF
        from  PSPAYCARD    C,
              PSPAYCARDPRD PR
        where rRECC.DOCUMENT = C.PSORGGRP
          and C.RN           = PR.PRN (+)
          and C.DATE_FROM   <= last_day(dCALCPER)
          and(C.DATE_TO     >= trunc(dCALCPER, 'MM') or C.DATE_TO is null)
          and(extract(year  from dCALCPER)  = PR.YEAR (+)
          and extract(month from dCALCPER) = PR.MONTH (+))
    )
    loop
      if CMP_NUM(rREC.C_RN, nOLD_C_RN)=0 then
        if nOLD_C_RN is not null then
          -- �������� ��������� ���������� ��������
          PKG_ENV.EPILOGUE( nCOMPANY, null, nOLD_CRN, 'ParentPayCards', 'PSPAYCARD_PSTS_CONFIRM', 'PSPAYCARD', nOLD_C_RN );
        end if;
        -- �������� ������ ���������� ��������
        PKG_ENV.PROLOGUE( nCOMPANY, null, rREC.CRN, 'ParentPayCards', 'PSPAYCARD_PSTS_CONFIRM', 'PSPAYCARD', rREC.C_RN );
      end if;

      if rREC.PR_RN is null then
        P_PSPAYCARDPRD_BASE_INSERT(nCOMPANY, rREC.C_RN, extract(year from dCALCPER), extract(month from dCALCPER), 1, rREC.PR_RN);
        nCONFIRMED:= nCONFIRMED+1;
      elsif rREC.PR_RN is not null and rREC.TAB_CONF=0 then
        update PSPAYCARDPRD PR
           set TAB_CONF = 1
         where PR.RN = rREC.PR_RN;
        nCONFIRMED:= nCONFIRMED+1;
      end if;
      nOLD_CRN:=  rREC.CRN;
      nOLD_C_RN:=  rREC.C_RN;
    end loop;
    if nOLD_C_RN is not null then
      -- �������� ��������� ���������� ��������
      PKG_ENV.EPILOGUE( nCOMPANY, null, nOLD_CRN, 'ParentPayCards', 'PSPAYCARD_PSTS_CONFIRM', 'PSPAYCARD', nOLD_C_RN );
    end if;
  end loop;
end;
/
show errors;

create or replace trigger T_PSPAYCARDHOUR_BINSERT
  before insert on PSPAYCARDHOUR for each row
declare
  nPSORGGRP       PKG_STD.tREF;
  sORGCODE        PSORG.CODE%type;
  sGRCODE         PSORGGRP.CODE%type;
  dWORKDATE       date;
begin
  /* ���������� ���������� ������ master-������� */
  select CD.COMPANY,
         CD.CRN,
         C.PSORGGRP,
         O.CODE   OCODE,
         OG.CODE  OGCODE,
         CD.WORKDATE
    into :new.COMPANY,
         :new.CRN,
         nPSORGGRP,
         sORGCODE,
         sGRCODE,
         dWORKDATE
    from PSPAYCARDDAY CD,
         PSPAYCARD    C,
         PSORG        O,
         PSORGGRP     OG
   where CD.RN      = :new.PRN
     and C.RN       = CD.PRN
     and C.PSORG    = O.RN
     and C.PSORGGRP = OG.RN;

  if :new.WORKEDHOURS!= 0 then
    P_EXCEPTION(abs(F_PSTS_CONFIRMED(:new.COMPANY, nPSORGGRP, dWORKDATE)-1),
      '���������� �������� ���� � ��������� �������� �� ������ "'||sGRCODE||'" ����������� "'||sORGCODE||'", �.�. �� ���� ������ ��� �������� ������.' );
    P_EXCEPTION(abs(F_PSTS_SAL_SHEET(:new.COMPANY, nPSORGGRP, dWORKDATE)-1),
      '���������� �������� ���� � ��������� �������� �� ������ "'||sGRCODE||'" ����������� "'||sORGCODE||'", �.�. �� ���� ������ ������������ ���������.' );
  end if;

  /* ����������� ������� */
  if ( PKG_IUD.PROLOGUE('PSPAYCARDHOUR', 'I') ) then
    PKG_IUD.REG_RN('RN', :new.RN);
    PKG_IUD.REG_COMPANY('COMPANY', :new.COMPANY);
    PKG_IUD.REG_CRN('CRN', :new.CRN);
    PKG_IUD.REG_PRN('PRN', :new.PRN);
    PKG_IUD.REG(1, 'HOURSTYPE', :new.HOURSTYPE);
    PKG_IUD.REG('WORKEDHOURS', :new.WORKEDHOURS);
    PKG_IUD.EPILOGUE;
  end if;
end;
/
show errors;

create or replace function F_PAN_PSORGGRP_CHECK_CLOSE
(
  nRN             in number,            -- RN ������ ����������
  dWORKDATE       in date               -- ���� ������
)
return number
as
  dCURRENT_PERIOD date;
  nRESULT         number(1);

begin
  -- ����� ��������� ������� ������
  begin
    select P.PERIOD
      into dCURRENT_PERIOD
      from PAN_PSORGGRPCP P
     where P.PRN = nRN
       and P.PERIOD = last_day(dWORKDATE);

     nRESULT := 1;
  exception
    when NO_DATA_FOUND then
       nRESULT := 0;
  end;

  return nRESULT;
end;
/
show errors;

create or replace procedure P_PAN_PSTSBRD_CHECK
(
  nPSPAYCARD      in number,            -- RN ��������� ��������
  dWORKDATE       in date               -- ���� �������
)
as
  nPSORGGRP       PKG_STD.tREF;
  sGROUP          PKG_STD.tSTRING;
  sORG            PKG_STD.tSTRING;

begin
  -- ���������� ����������� � ������ ��������� ��������
  begin
    select PC.PSORGGRP,
           G.CODE,
           O.CODE
      into nPSORGGRP,
           sGROUP,
           sORG
      from PSPAYCARD PC,
           PSORGGRP G,
           PSORG O
     where PC.RN = nPSPAYCARD
       and PC.PSORGGRP = G.RN
       and G.PRN = O.RN;
  exception
    when NO_DATA_FOUND then
      PKG_MSG.RECORD_NOT_FOUND(nPSPAYCARD, 'ParentPayCards');
  end;

  -- �������� �������� �������� ���������� �������
  if F_PAN_PSORGGRP_CHECK_CLOSE(nPSORGGRP, dWORKDATE) = 1 then
    P_EXCEPTION(0, '������ ������ "%s" ���������� "%s" ������ ��� ����������� � ������� %s. ��� ����������� ��������� ������� ������.',
      sGROUP, sORG, to_char(dWORKDATE, 'mm/yyyy'));
  end if;
end;
/
show errors;

create or replace triggeR T_PAN_PSPAYCARDDAY_BUPDATE
  before update on PSPAYCARDDAY for each row
begin
  if cmp_dat( :new.WORKDATE, :old.WORKDATE ) = 0 then
    P_PAN_PSTSBRD_CHECK(:new.PRN, :new.WORKDATE);
  end if;
end;
/
show errors;

create or replace trigger T_PAN_PSPAYCARDHOUR_BISNERT
  before insert on PSPAYCARDHOUR for each row
declare
  nPSPAYCARD      PKG_STD.tREF;
  dWORKDATE       date;
begin
  select D.PRN,
         D.WORKDATE
    into nPSPAYCARD,
         dWORKDATE
    from PSPAYCARDDAY D
   where D.RN = :new.PRN;

  P_PAN_PSTSBRD_CHECK(nPSPAYCARD, dWORKDATE);
end;
/
show errors;

create or replace trigger T_PAN_PSPAYCARDHOUR_BUPDATE
  before update on PSPAYCARDHOUR for each row
declare
  nPSPAYCARD      PKG_STD.tREF;
  dWORKDATE       date;
begin
  if cmp_num(:new.WORKEDHOURS, :old.WORKEDHOURS) = 0 then
    select D.PRN,
           D.WORKDATE
      into nPSPAYCARD,
           dWORKDATE
      from PSPAYCARDDAY D
     where D.RN = :new.PRN;

    P_PAN_PSTSBRD_CHECK(nPSPAYCARD, dWORKDATE);
  end if;
end;
/
show errors;

create or replace trigger T_PAN_PSPAYCARDDAY_BINSERT
  before insert on PSPAYCARDDAY for each row
begin
  P_PAN_PSTSBRD_CHECK(:new.PRN, :new.WORKDATE);
end;
/
show errors;

create or replace procedure UDO_P_TIMESHEET_RECEIVE
-- ��������� ������ ������������ �� CSV �����
(
  nCOMPANY        in number,            -- �����������
  cDATA           in clob,              -- ������ ������������ ������ � ������� CSV
  sMESSAGE        out varchar2          -- ���������
)
as
  sLINE           varchar2(32767);
  nLINE_NUMBER    pls_integer := 1;
  dPERIOD         date;
  sAGNFAMILYNAME  PKG_STD.tSTRING;
  sAGNFIRSTNAME   PKG_STD.tSTRING;
  SAGNLASTNAME    PKG_STD.tSTRING;
  dAGNBURN        date;
  sPHONE          PKG_STD.tSTRING;
  sPHONE2         PKG_STD.tSTRING;
  dDATE_FROM      date;
  dDATE_TO        date;
  dPERIOD_BEGIN   date;
  nORG_RN         PKG_STD.tREF;
  nGROUP_RN       PKG_STD.tREF;
  nPAYCARD_RN     PKG_STD.tREF;
  nERROR_COUNT    pls_integer := 0;
  nSUCCESS_COUNT  pls_integer := 0;
  nDAYS_IN_MONTH  pls_integer;
  nMONTH          pls_integer;
  nYEAR           pls_integer;
  nHOURSTYPE      PKG_STD.tREF;
  sHOURSTYPE      PKG_STD.tSTRING;
  dDATE           date;
  nPAYCARDDAY     PKG_STD.tREF;
  nPAYCARDHOUR    PKG_STD.tREF;
  nWORKEDHOURS    PKG_STD.tSUMM;
  nHOURS_FACT     PKG_STD.tSUMM;
  sORG_CODE       PKG_STD.tSTRING;
  sORG_INN        PKG_STD.tSTRING;
  sGROUP_CODE     PKG_STD.tSTRING;
  sPERION         PKG_STD.tSTRING;
  l_amount        pls_integer := 1;
  l_length        pls_integer;
  token           varchar2(4000);
  sobch           varchar2(4000);
  sERROR          PKG_STD.tSTRING;
  nDAYSTYPE_B     PKG_STD.tREF;
  nDAYSTYPE_O     PKG_STD.tREF;
  nDAYSTYPE_NU    PKG_STD.tREF;
  nDAYSTYPE_N     PKG_STD.tREF;
  sDAYSTYPE_CODE  PKG_STD.tSTRING;
  nENPERIOD       PKG_STD.tREF;
  nTMP            pls_integer;
  nSCHEDULE_HOURS PKG_STD.tSUMM;

  -- ��������� ������ �����
  procedure PERFORM_LINE
  (
    nLINE_NUMBER  in number,
    sSOURCE_LINE  in varchar2
  )
  as
    sLINE         PKG_STD.tSTRING := sSOURCE_LINE;
    sDAY_VALUE    PKG_STD.tSTRING;
    nDAYSTYPE     PKG_STD.tREF;
  begin
    -- �������� ������ ";" � ����� ������
    while substr(sLINE, -1) = ';' loop
      sLINE          := substr(sLINE, 1, length(sLINE) - 1);
    end loop;
    sLINE := sLINE||';';

    -- ������� ������
    if nLINE_NUMBER = 1 then
      sPERION        := UDO_F_GET_LIST_ITEM(sLINE, 1, ';');
      dPERIOD        := UDO_F_S2D(sPERION);
      dPERIOD_BEGIN  := trunc(dPERIOD, 'month');
      nDAYS_IN_MONTH := extract(day from last_day(dPERIOD));
      nMONTH         := extract(month from dPERIOD);
      nYEAR          := extract(year from dPERIOD);

    -- �����������
    elsif nLINE_NUMBER = 2 then
      sORG_CODE := UDO_F_GET_LIST_ITEM(sLINE, 1, ';');
      sORG_INN := UDO_F_GET_LIST_ITEM(sLINE, 2, ';');

      begin
        select O.RN
          into nORG_RN
          from PSORG O
         where O.COMPANY = nCOMPANY
           and O.CODE = sORG_CODE;
      exception
        when NO_DATA_FOUND then
          P_EXCEPTION(0, '����������� "%s" �� �������', sORG_CODE);
      end;

    -- ������
    elsif nLINE_NUMBER = 3 then
      sGROUP_CODE := UDO_F_GET_LIST_ITEM(sLINE, 1, ';');

      begin
        select G.RN
          into nGROUP_RN
          from PSORGGRP G
         where G.PRN = nORG_RN
           and G.CODE = sGROUP_CODE;
      exception
        when NO_DATA_FOUND then
          P_EXCEPTION(0, '������ "%s" � ����������� "%s" �� �������', sGROUP_CODE, sORG_CODE);
      end;

    -- ������������ ������� � ������
    elsif nLINE_NUMBER > 4 and trim(sLINE) is not null then
      sAGNFAMILYNAME := UDO_F_GET_LIST_ITEM(sLINE, 1, ';');
      sAGNFIRSTNAME  := UDO_F_GET_LIST_ITEM(sLINE, 2, ';');
      SAGNLASTNAME   := UDO_F_GET_LIST_ITEM(sLINE, 3, ';');
      dAGNBURN       := UDO_F_S2D(UDO_F_GET_LIST_ITEM(sLINE, 4, ';'));
      sPHONE         := UDO_F_GET_LIST_ITEM(sLINE, 5, ';');
      sPHONE2        := UDO_F_GET_LIST_ITEM(sLINE, 6, ';');
      dDATE_FROM     := UDO_F_S2D(UDO_F_GET_LIST_ITEM(sLINE, 7, ';'));
      dDATE_TO       := UDO_F_S2D(UDO_F_GET_LIST_ITEM(sLINE, 8, ';'));

      -- ����� ��������� ��������
      begin
        select PC.RN
          into nPAYCARD_RN
          from PSPAYCARD PC,
               PSPERSCARD C,
               AGNLIST A
         where PC.PSORGGRP = nGROUP_RN
           and PC.DATE_FROM <= last_day(dPERIOD)
           and (PC.DATE_TO is null or PC.DATE_TO >= dPERIOD_BEGIN)
           and PC.PERSCARD = C.RN
           and C.AGENT = A.RN
           and trim(A.AGNFAMILYNAME) = trim(sAGNFAMILYNAME)
           and trim(A.AGNFIRSTNAME) = trim(sAGNFIRSTNAME)
           and cmp_vc2(trim(A.AGNLASTNAME), trim(sAGNLASTNAME)) = 1
           and cmp_dat(A.AGNBURN, dAGNBURN) = 1;
      exception
        when NO_DATA_FOUND then
          begin
            select PC.RN
              into nPAYCARD_RN
              from PSPAYCARD PC,
                   PSPERSCARD C,
                   AGNLIST A
             where PC.PSORGGRP = nGROUP_RN
               and PC.DATE_FROM <= last_day(dPERIOD)
               and (PC.DATE_TO is null or PC.DATE_TO >= dPERIOD_BEGIN)
               and PC.PERSCARD = C.RN
               and C.AGENT = A.RN
               and trim(A.AGNFAMILYNAME) = trim(sAGNFAMILYNAME)
               and trim(A.AGNFIRSTNAME) = trim(sAGNFIRSTNAME);
          exception
            when NO_DATA_FOUND then
              nERROR_COUNT := nERROR_COUNT + 1;
              sERROR := substr(sERROR||FORMAT_TEXT('%s. %s %s �� ������(�)'||chr(10),
                to_char(nERROR_COUNT), sAGNFAMILYNAME, sAGNFIRSTNAME), 1, 4000);
              return;
          end;
      end;

      -- �������� �������� ������� � ������
      P_PAN_PSTSBRD_CHECK(nPAYCARD_RN, dPERIOD_BEGIN);
      -- �������� ����������� ������
      P_EXCEPTION(abs(F_PSTS_CONFIRMED(nCOMPANY, nGROUP_RN, dDATE)-1),
        '���������� ��������� ��� ��� � ��������� �������� �� ������ "%s" ����������� "%s", �.�. �� ���� ������ ��� �������� ������.',
        sGROUP_CODE, sORG_CODE);
      -- �������� ������� ���������
      P_EXCEPTION(abs(F_PSTS_SAL_SHEET(nCOMPANY, nGROUP_RN, dDATE)-1),
        '���������� ��������� ��� ��� � ��������� �������� �� ������ "%s" ����������� "%s", �.�. �� ���� ������ ������������ ���������.',
        sGROUP_CODE, sORG_CODE);

      -- �������� ���� ��� �������: ������� ��������� �� ������� ����� � ������� �������
      begin
        select EP.RN,
               (
                 select nvl(sum(SD.HOURS_RATE), 0)
                   from SLSTRSCHEDULE SS,
                        SLSCHEDDATE SD
                  where SS.PRN = PC.SLSCHEDULE
                    and SD.PRN = SS.RN
               ) SCHEDULE_HOURS
          into nENPERIOD,
               nSCHEDULE_HOURS
          from PSPAYCARD PC,
               ENPERIOD EP
         where PC.RN = nPAYCARD_RN
           and dPERIOD between EP.STARTDATE and EP.ENDDATE
           and EP.SCHEDULE = PC.SLSCHEDULE;

        -- ��������� ������ ��� ������� � ������ ����� 60 = 5 ���� * 12 �����
        if nSCHEDULE_HOURS != 60 then
          nENPERIOD := null;
        end if;
      exception
        when NO_DATA_FOUND then
          nENPERIOD := null;
      end;

      -- ������������ �� ����
      for d in 1 .. nDAYS_IN_MONTH loop
        dDATE := int2date(d, nMONTH, nYEAR);
        sDAY_VALUE := UDO_F_GET_LIST_ITEM(sLINE, 8 + d, ';');

        -- ������ �� ������� �
        if sDAY_VALUE = '�' then
          nHOURS_FACT := 0;
          nDAYSTYPE := nDAYSTYPE_B;
        -- ������ �
        elsif sDAY_VALUE = '�' then
          nHOURS_FACT := 0;
          nDAYSTYPE := nDAYSTYPE_O;
        -- ������ �� ������������ ������� ��
        elsif sDAY_VALUE = '��' then
          nHOURS_FACT := 0;
          nDAYSTYPE := nDAYSTYPE_NU;
        -- ������ �� �������������� ������� ��
        elsif instr(sDAY_VALUE, '��') > 0 then
          nHOURS_FACT := nvl(UDO_F_S2N(F_PAN_DROP_LITERS(sDAY_VALUE)), 0);
          nDAYSTYPE := nDAYSTYPE_N;
        -- ����
        else
          nHOURS_FACT := nvl(UDO_F_S2N(sDAY_VALUE), 0);
          nDAYSTYPE := null;
        end if;

        if nHOURS_FACT > 0 or nDAYSTYPE is not null then
          nPAYCARDDAY := PKG_PSPAYCARDTIME.CREATE_DAY(nCOMPANY, nPAYCARD_RN, dDATE, nDAYSTYPE, 1);

          -- �������� ��� ������ �� �������� ���������
          if nENPERIOD is not null then
            begin
              select 1
                into nTMP
                from WORKDAYS D
               where D.PRN = nENPERIOD
                 and D.DAYS = d
                 and exists
                     (
                       select 1
                         from WORKDAYSTR DS
                        where DS.PRN = D.RN
                          and DS.HOURSNORM != 0
                     );
            exception
              when NO_DATA_FOUND then
                nERROR_COUNT := nERROR_COUNT + 1;
                sERROR := substr(sERROR||FORMAT_TEXT('%s. %s %s ���� %s ��� �������.'||chr(10),
                  to_char(nERROR_COUNT), sAGNFAMILYNAME, sAGNFIRSTNAME, to_char(dDATE, 'dd.mm.yyyy')), 1, 4000);
                continue;
            end;
          end if;

          begin
            select H.RN,
                   H.WORKEDHOURS
              into nPAYCARDHOUR,
                   nWORKEDHOURS
              from PSPAYCARDHOUR H
             where H.PRN = nPAYCARDDAY
               and H.HOURSTYPE = nHOURSTYPE;

            if nWORKEDHOURS != nHOURS_FACT then
              update PSPAYCARDHOUR H
                 set H.WORKEDHOURS = nHOURS_FACT
               where H.PRN = nPAYCARDDAY
                 and H.HOURSTYPE = nHOURSTYPE;
            end if;
          exception
            when NO_DATA_FOUND then
              begin
                P_PSPAYCARDHOUR_BASE_INSERT
                (
                  nCOMPANY     => nCOMPANY,     -- �����������
                  nPRN         => nPAYCARDDAY,  -- RN ���
                  nHOURSTYPE   => nHOURSTYPE,   -- ��� ����
                  nWORKEDHOURS => nHOURS_FACT,  -- ���������� �����
                  nRN          => nPAYCARDHOUR  -- RN ����
                );
              exception
                when OTHERS then
                  P_EXCEPTION(0, '������ �������� ������ � �����: '||ERROR_TEXT);
              end;
          end;
        else
          delete
            from PSPAYCARDDAY D
           where D.PRN = nPAYCARD_RN
             and D.WORKDATE = dDATE;
        end if;
      end loop;
      nSUCCESS_COUNT := nSUCCESS_COUNT + 1;
    end if;
  end;

begin
  begin
    -- ��� ����
    begin
      select T.RN,
             T.CODE
        into nHOURSTYPE,
             sHOURSTYPE
        from SL_HOURS_TYPES T
       where T.BASE_SIGN = 1
         and substr(upper(T.SHORT_CODE), 1, 1) = '�';
    exception
      when NO_DATA_FOUND then
        P_EXCEPTION(0, '�������� ��� ���� � ����� � �� ������');
    end;

    -- ���� ���
    FIND_SLDAYSTYPE_SHORTCODE(0, 0, nCOMPANY, '�', sDAYSTYPE_CODE, nDAYSTYPE_B);
    FIND_SLDAYSTYPE_SHORTCODE(0, 0, nCOMPANY, '�', sDAYSTYPE_CODE, nDAYSTYPE_O);
    FIND_SLDAYSTYPE_SHORTCODE(0, 0, nCOMPANY, '��', sDAYSTYPE_CODE, nDAYSTYPE_NU);
    FIND_SLDAYSTYPE_SHORTCODE(0, 0, nCOMPANY, '��', sDAYSTYPE_CODE, nDAYSTYPE_N);

    l_length := dbms_lob.getlength(cDATA);
    for i in 1..l_length loop
      DBMS_LOB.READ
      (
        LOB_LOC => cDATA,
        AMOUNT => l_amount,
        OFFSET => i,
        BUFFER => token
      );
      if token = chr(10) then
        PERFORM_LINE(nLINE_NUMBER, sobch);
        nLINE_NUMBER := nLINE_NUMBER + 1;
        sobch:= null;
      elsif i = l_length then
        sobch:= sobch||token;
        PERFORM_LINE(nLINE_NUMBER, sobch);
      else
        sobch:= sobch||token;
      end if;
    end loop;

    if nERROR_COUNT = 0 then
      sMESSAGE := '������ ������������ ������� �������� � �����';
    else
      sMESSAGE := substr(FORMAT_TEXT('��������� ������: %s������ ��������:%s', to_char(nSUCCESS_COUNT)||chr(10), chr(10)||sERROR), 1, 4000);
    end if;
  exception
    when OTHERS then
      sMESSAGE := ERROR_TEXT;
  end;
end;
/
show errors;
create or replace public synonym UDO_P_TIMESHEET_RECEIVE for UDO_P_TIMESHEET_RECEIVE;
grant execute on UDO_P_TIMESHEET_RECEIVE to public;

create or replace procedure UDO_P_PSORGGRP_LOAD
-- �������� ������ ������������ ������ �� CSV �����
(
  nCOMPANY        in number,            -- �����������
  nIDENT          in number,            -- ������������� ���������� ������
  sMESSAGE        out varchar2          -- ���������
)
as
  cDATA           clob;
begin
  -- �������� ����� �� ������
  begin
    select B.DATA
      into cDATA
      from FILE_BUFFER B
     where B.IDENT = nIDENT;
  exception
    when NO_DATA_FOUND then
      P_EXCEPTION(0, '������ ���� �������� CSV ���� � �������� �����.');
    when TOO_MANY_ROWS then
      P_EXCEPTION(0, '������ ���� �������� ������ ���� CSV ���� � �������� �����.');
  end;

  -- ��������� �����
  UDO_P_TIMESHEET_RECEIVE(nCOMPANY, cDATA, sMESSAGE);
end;
/
show errors;
create or replace public synonym UDO_P_PSORGGRP_LOAD for UDO_P_PSORGGRP_LOAD;
grant execute on UDO_P_PSORGGRP_LOAD to public;

create or replace procedure UDO_P_TIMESHEET_SEND
-- �������� ������ ������������ � ������� CSV
(
  nORG_RN         in number,            -- RN �����������
  sGROUP          in varchar2,          -- �������� ������
  dPERIOD         in date,              -- ������ ������ ������������
  sFILENAME       out varchar2,         -- ��� ����� CSV
  cDATA           out clob              -- ������ ������������ ������ � ������� CSV
)
as
  dPERIOD_        date := trunc(sysdate);
  nDAYS_IN_MONTH  binary_integer := D_DAY(last_day(dPERIOD_));
  nMONTH          binary_integer := D_MONTH(dPERIOD_);
  nYEAR           binary_integer := D_YEAR(dPERIOD_);
  sPERIOD         PKG_STD.tSTRING := upper(F_GET_MONTH(nMONTH))||' '||nYEAR;
  sTEXT           PKG_STD.tSTRING;
  CR              varchar2(1) := chr(10);
  nWORKEDHOURS    PKG_STD.tSUMM;
  sDAYSTYPE       PKG_STD.tSTRING;
  sDAY_VALUE      PKG_STD.tSTRING;
begin
  -- �������� ������
  DBMS_LOB.CREATETEMPORARY(cDATA, true);

  -- ������, ����������� � ������
  for cur in
  (
    select O.CODE ORG_CODE,
           A.AGNIDNUMB,
           G.CODE GROUP_CODE,
           UDO_F_SLSCHEDULE_MNEMOCODE(G.SHEDULE) SLSCHEDULE_CODE,
           decode(lower(K.CODE), '����', 1, 2) MEALS,
           G.RN GROUP_RN
      from PSORG O,
           AGNLIST A,
           PSORGGRP G,
           PSGRPKND K
     where O.RN = nORG_RN
       and O.AGENT = A.RN
       and G.PRN = O.RN
       and G.CODE = sGROUP
       and G.GROUPKND = K.RN
  )
  loop
    -- ���������
    sFILENAME := UDO_F_MAKE_FILE_NAME(
      format_text('%s_%s_%s_�����', cur.ORG_CODE, replace(cur.GROUP_CODE, '/', '-'), sPERIOD),
      'csv'
    );
    sTEXT := sPERIOD||';'||CR||
             cur.ORG_CODE||';'||cur.AGNIDNUMB||';'||CR||
             cur.GROUP_CODE||';'||cur.SLSCHEDULE_CODE||';'||cur.MEALS||';'||CR;
    DBMS_LOB.WRITEAPPEND(cDATA, length(sTEXT), sTEXT);
    sTEXT := '�������;���;��������;���� ��������;������� 1;������� 2;���� �����������;���� �������;';
    for d in 1..nDAYS_IN_MONTH loop
      sTEXT := sTEXT||d||';';
    end loop;
    sTEXT := sTEXT||CR;
    DBMS_LOB.WRITEAPPEND(cDATA, length(sTEXT), sTEXT);

    -- ������ ������������
    for card in
    (
      select A1.AGNFAMILYNAME,
             A1.AGNFIRSTNAME,
             A1.AGNLASTNAME,
             to_char(A1.AGNBURN, 'dd.mm.yyyy') AGNBURN,
             nvl(A1.PHONE, A2.PHONE) PHONE,
             nvl(A1.PHONE2, A2.PHONE2) PHONE2,
             to_char(PC.DATE_FROM, 'dd.mm.yyyy') DATE_FROM,
             to_char(PC.DATE_TO, 'dd.mm.yyyy') DATE_TO,
             PC.RN PAYCARD_RN
        from PSPAYCARD PC,
             PSPERSCARD C,
             AGNLIST A1,
             AGNLIST A2
       where PC.PSORGGRP = cur.GROUP_RN
         and PC.DATE_FROM <= last_day(dPERIOD_)
         and (PC.DATE_TO is null or PC.DATE_TO >= trunc(dPERIOD_, 'month'))
         and PC.PERSCARD = C.RN
         and C.AGENT = A1.RN
         and C.PAYER = A2.RN(+)
       order by
             A1.AGNFAMILYNAME,
             A1.AGNFIRSTNAME,
             A1.AGNLASTNAME,
             A1.AGNBURN
    )
    loop
      -- ������� � ������
      sTEXT := card.AGNFAMILYNAME||';'||card.AGNFIRSTNAME||';'||card.AGNLASTNAME||';'||card.AGNBURN||';'||
               card.PHONE||';'||card.PHONE2||';'||card.DATE_FROM||';'||card.DATE_TO||';';

      -- ������������ �� ����
      for d in 1 .. nDAYS_IN_MONTH loop
        begin
          select H.WORKEDHOURS,
                 DT.SHORT_CODE
            into nWORKEDHOURS,
                 sDAYSTYPE
            from PSPAYCARDDAY D,
                 SLDAYSTYPE DT,
                 PSPAYCARDHOUR H,
                 SL_HOURS_TYPES T
           where D.PRN = card.PAYCARD_RN
             and D.DAYSTYPE = DT.RN(+)
             and D.WORKDATE = int2date(d, nMONTH, nYEAR)
             and D.RN = H.PRN
             and H.HOURSTYPE = T.RN
             and T.BASE_SIGN = 1
             and substr(upper(T.SHORT_CODE), 1, 1) = '�';

          if sDAYSTYPE in ('�', '�', '��') then
            sDAY_VALUE := sDAYSTYPE;
          elsif sDAYSTYPE = '��' then
            sDAY_VALUE := nWORKEDHOURS||sDAYSTYPE;
          else
            sDAY_VALUE := nWORKEDHOURS;
          end if;

          sTEXT := sTEXT||sDAY_VALUE;
        exception
          when NO_DATA_FOUND then
            null;
        end;
        sTEXT := sTEXT||';';
      end loop;
      sTEXT := sTEXT||CR;
      DBMS_LOB.WRITEAPPEND(cDATA, length(sTEXT), sTEXT);
    end loop;
  end loop;
end;
/
show errors;
create or replace public synonym UDO_P_TIMESHEET_SEND for UDO_P_TIMESHEET_SEND;
grant execute on UDO_P_TIMESHEET_SEND to public;

create or replace procedure UDO_P_PSORGGRP_UNLOAD
-- �������� ������ ������������ � ������� CSV
(
  nIDENT          in number,            -- ������������� ���������� ������
  dPERIOD         in date               -- ������ ������ ������������
)
as
  cDATA           clob;
  sFILENAME       PKG_STD.tSTRING;
  nCOUNT          binary_integer;
begin
  -- �������� ������
  DBMS_LOB.CREATETEMPORARY(cDATA, true);

  -- �������� ���������� ���������� �����
  select count(*)
    into nCOUNT
    from SELECTLIST SL
   where SL.IDENT = nIDENT;

  if nCOUNT != 1 then
    P_EXCEPTION(0, '������ ���� �������� ���� ������.');
  end if;

  -- �������� ������ ������������ ������ � ������� CSV
  for cur in
  (
    select G.PRN ORG_RN,
           G.CODE GROUP_CODE
      from SELECTLIST SL,
           PSORGGRP G
     where SL.IDENT = nIDENT
       and SL.DOCUMENT = G.RN
  )
  loop
    UDO_P_TIMESHEET_SEND
    (
      nORG_RN         => cur.ORG_RN,        -- RN �����������
      sGROUP          => cur.GROUP_CODE,    -- �������� ������
      dPERIOD         => dPERIOD,           -- ������ ������ ������������
      sFILENAME       => sFILENAME,         -- ��� ����� CSV
      cDATA           => cDATA              -- ������ ������������ ������ � ������� CSV
    );
  end loop;

  -- ������ ������
  P_FILE_BUFFER_INSERT(nIDENT, sFILENAME, cDATA, null);

  -- ������������ ������
  DBMS_LOB.FREETEMPORARY(cDATA);
end;
/
show errors;
create or replace public synonym UDO_P_PSORGGRP_UNLOAD for UDO_P_PSORGGRP_UNLOAD;
grant execute on UDO_P_PSORGGRP_UNLOAD to public;

create or replace procedure UDO_P_PSORG_GET_GROUPS
-- ��������� ������ ����� ����������
(
  nORG_RN         in number,            -- RN ����������
  sGROUPS         out varchar2          -- ������ ���������� ����� ����� ";"
)
as
begin
  for cur in
  (
    select G.CODE
      from PSORGGRP G
     where G.PRN = nORG_RN
       and G.DATE_TO is null
     order by 1
  )
  loop
    sGROUPS := sGROUPS||cur.CODE||';';
  end loop;
  sGROUPS := substr(sGROUPS, 1, length(sGROUPS)-1);
end;
/
show errors;
create or replace public synonym UDO_P_PSORG_GET_GROUPS for UDO_P_PSORG_GET_GROUPS;
grant execute on UDO_P_PSORG_GET_GROUPS to public;

create or replace procedure UDO_P_GET_PSORGS
-- ��������� ������ ���������� �� ���
(
  sINN            in varchar2,          -- ��� ����������
  sORGS_JSON      out varchar2          -- ������ ���������� � ������� JSON
)
as
begin
  for cur in
  (
    select O.RN,
           replace(O.CODE, '"', '\"') CODE,
           replace(O.NAME, '"', '\"') NAME,
           O.COMPANY
      from PSORG O,
           AGNLIST A,
           COMPANIES C,
           AGNLIST CA
     where O.AGENT = A.RN
       and A.AGNIDNUMB = sINN
       and O.COMPANY = C.RN
       and C.AGENT = CA.RN
       and exists
           (
             select *
               from PSORGGRP G
              where G.PRN = O.RN
                and G.DATE_TO is null
           )
  )
  loop
    sORGS_JSON := sORGS_JSON||FORMAT_TEXT(
      '{"org_rn": %s, "org_code": "%s", "org_name": "%s", "org_inn": "%s", "company_rn": %s}, ',
      cur.RN, cur.CODE, cur.NAME, sINN, cur.COMPANY);
  end loop;

  if sORGS_JSON is not null then
    sORGS_JSON := substr(sORGS_JSON, 1, length(sORGS_JSON)-2);
  end if;

  sORGS_JSON := '['||sORGS_JSON||']';
end;
/
show errors;
create or replace public synonym UDO_P_GET_PSORGS for UDO_P_GET_PSORGS;
grant execute on UDO_P_GET_PSORGS to public;

create or replace procedure UDO_P_GET_PSORG
-- ��������� ���������� �� ��� � ����������� ������
(
  sINN            in varchar2,          -- ��� ����������
  sGROUP          in varchar2,          -- �������� ������
  sORG_JSON       out varchar2          -- ������ ���������� � ������� JSON
)
as
begin
  for cur in
  (
    select O.RN,
           replace(O.CODE, '"', '\"') CODE,
           replace(O.NAME, '"', '\"') NAME,
           O.COMPANY
      from PSORG O,
           AGNLIST A,
           COMPANIES C,
           AGNLIST CA
     where O.AGENT = A.RN
       and A.AGNIDNUMB = sINN
       and O.COMPANY = C.RN
       and C.AGENT = CA.RN
       and exists
           (
             select *
               from PSORGGRP G
              where G.PRN = O.RN
                and G.CODE = sGROUP
           )
  )
  loop
    sORG_JSON := sORG_JSON||FORMAT_TEXT(
      '{"org_rn": %s, "org_code": "%s", "org_name": "%s", "org_inn": "%s", "company_rn": %s}, ',
      cur.RN, cur.CODE, cur.NAME, sINN, cur.COMPANY);
  end loop;
  sORG_JSON := substr(sORG_JSON, 1, length(sORG_JSON)-2);
end;
/
show errors;
create or replace public synonym UDO_P_GET_PSORG for UDO_P_GET_PSORG;
grant execute on UDO_P_GET_PSORG to public;

create or replace trigger T_PSTSBRD_BINSERT
  before insert on PSTSBRD for each row
begin
  :new.F1 := PKG_EXT.IIF( :new.F1 = 0, null, :new.F1 );
  :new.F2 := PKG_EXT.IIF( :new.F2 = 0, null, :new.F2 );
  :new.F3 := PKG_EXT.IIF( :new.F3 = 0, null, :new.F3 );
  :new.F4 := PKG_EXT.IIF( :new.F4 = 0, null, :new.F4 );
  :new.F5 := PKG_EXT.IIF( :new.F5 = 0, null, :new.F5 );
  :new.F6 := PKG_EXT.IIF( :new.F6 = 0, null, :new.F6 );
  :new.F7 := PKG_EXT.IIF( :new.F7 = 0, null, :new.F7 );
  :new.F8 := PKG_EXT.IIF( :new.F8 = 0, null, :new.F8 );
  :new.F9 := PKG_EXT.IIF( :new.F9 = 0, null, :new.F9 );
  :new.F10 := PKG_EXT.IIF( :new.F10 = 0, null, :new.F10 );
  :new.F11 := PKG_EXT.IIF( :new.F11 = 0, null, :new.F11 );
  :new.F12 := PKG_EXT.IIF( :new.F12 = 0, null, :new.F12 );
  :new.F13 := PKG_EXT.IIF( :new.F13 = 0, null, :new.F13 );
  :new.F14 := PKG_EXT.IIF( :new.F14 = 0, null, :new.F14 );
  :new.F15 := PKG_EXT.IIF( :new.F15 = 0, null, :new.F15 );
  :new.F16 := PKG_EXT.IIF( :new.F16 = 0, null, :new.F16 );
  :new.F17 := PKG_EXT.IIF( :new.F17 = 0, null, :new.F17 );
  :new.F18 := PKG_EXT.IIF( :new.F18 = 0, null, :new.F18 );
  :new.F19 := PKG_EXT.IIF( :new.F19 = 0, null, :new.F19 );
  :new.F20 := PKG_EXT.IIF( :new.F20 = 0, null, :new.F20 );
  :new.F21 := PKG_EXT.IIF( :new.F21 = 0, null, :new.F21 );
  :new.F22 := PKG_EXT.IIF( :new.F22 = 0, null, :new.F22 );
  :new.F23 := PKG_EXT.IIF( :new.F23 = 0, null, :new.F23 );
  :new.F24 := PKG_EXT.IIF( :new.F24 = 0, null, :new.F24 );
  :new.F25 := PKG_EXT.IIF( :new.F25 = 0, null, :new.F25 );
  :new.F26 := PKG_EXT.IIF( :new.F26 = 0, null, :new.F26 );
  :new.F27 := PKG_EXT.IIF( :new.F27 = 0, null, :new.F27 );
  :new.F28 := PKG_EXT.IIF( :new.F28 = 0, null, :new.F28 );
  :new.F29 := PKG_EXT.IIF( :new.F29 = 0, null, :new.F29 );
  :new.F30 := PKG_EXT.IIF( :new.F30 = 0, null, :new.F30 );
  :new.F31 := PKG_EXT.IIF( :new.F31 = 0, null, :new.F31 );

  :new.FD := PKG_EXT.IIF( :new.FD = 0, null, :new.FD );
  :new.FH := PKG_EXT.IIF( :new.FH = 0, null, :new.FH );

  if :new.F1 is not null and :new.D1 is not null and :new.D1 != '��' then
    P_EXCEPTION(0, '1 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F1 is null and :new.D1 = '��' then
    P_EXCEPTION(0, '1 �����: ��� �� ��������� ����.');
  end if;

  if :new.F2 is not null and :new.D2 is not null and :new.D2 != '��' then
    P_EXCEPTION(0, '2 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F2 is null and :new.D2 = '��' then
    P_EXCEPTION(0, '2 �����: ��� �� ��������� ����.');
  end if;

  if :new.F3 is not null and :new.D3 is not null and :new.D3 != '��' then
    P_EXCEPTION(0, '3 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F3 is null and :new.D3 = '��' then
    P_EXCEPTION(0, '3 �����: ��� �� ��������� ����.');
  end if;

  if :new.F4 is not null and :new.D4 is not null and :new.D4 != '��' then
    P_EXCEPTION(0, '4 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F4 is null and :new.D4 = '��' then
    P_EXCEPTION(0, '4 �����: ��� �� ��������� ����.');
  end if;

  if :new.F5 is not null and :new.D5 is not null and :new.D5 != '��' then
    P_EXCEPTION(0, '5 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F5 is null and :new.D5 = '��' then
    P_EXCEPTION(0, '5 �����: ��� �� ��������� ����.');
  end if;

  if :new.F6 is not null and :new.D6 is not null and :new.D6 != '��' then
    P_EXCEPTION(0, '6 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F6 is null and :new.D6 = '��' then
    P_EXCEPTION(0, '6 �����: ��� �� ��������� ����.');
  end if;

  if :new.F7 is not null and :new.D7 is not null and :new.D7 != '��' then
    P_EXCEPTION(0, '7 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F7 is null and :new.D7 = '��' then
    P_EXCEPTION(0, '7 �����: ��� �� ��������� ����.');
  end if;

  if :new.F8 is not null and :new.D8 is not null and :new.D8 != '��' then
    P_EXCEPTION(0, '8 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F8 is null and :new.D8 = '��' then
    P_EXCEPTION(0, '8 �����: ��� �� ��������� ����.');
  end if;

  if :new.F9 is not null and :new.D9 is not null and :new.D9 != '��' then
    P_EXCEPTION(0, '9 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F9 is null and :new.D9 = '��' then
    P_EXCEPTION(0, '9 �����: ��� �� ��������� ����.');
  end if;

  if :new.F10 is not null and :new.D10 is not null and :new.D10 != '��' then
    P_EXCEPTION(0, '10 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F10 is null and :new.D10 = '��' then
    P_EXCEPTION(0, '10 �����: ��� �� ��������� ����.');
  end if;

  if :new.F11 is not null and :new.D11 is not null and :new.D11 != '��' then
    P_EXCEPTION(0, '11 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F11 is null and :new.D11 = '��' then
    P_EXCEPTION(0, '11 �����: ��� �� ��������� ����.');
  end if;

  if :new.F12 is not null and :new.D12 is not null and :new.D12 != '��' then
    P_EXCEPTION(0, '12 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F12 is null and :new.D12 = '��' then
    P_EXCEPTION(0, '12 �����: ��� �� ��������� ����.');
  end if;

  if :new.F13 is not null and :new.D13 is not null and :new.D13 != '��' then
    P_EXCEPTION(0, '13 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F13 is null and :new.D13 = '��' then
    P_EXCEPTION(0, '13 �����: ��� �� ��������� ����.');
  end if;

  if :new.F14 is not null and :new.D14 is not null and :new.D14 != '��' then
    P_EXCEPTION(0, '14 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F14 is null and :new.D14 = '��' then
    P_EXCEPTION(0, '14 �����: ��� �� ��������� ����.');
  end if;

  if :new.F15 is not null and :new.D15 is not null and :new.D15 != '��' then
    P_EXCEPTION(0, '15 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F15 is null and :new.D15 = '��' then
    P_EXCEPTION(0, '15 �����: ��� �� ��������� ����.');
  end if;

  if :new.F16 is not null and :new.D16 is not null and :new.D16 != '��' then
    P_EXCEPTION(0, '16 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F16 is null and :new.D16 = '��' then
    P_EXCEPTION(0, '16 �����: ��� �� ��������� ����.');
  end if;

  if :new.F17 is not null and :new.D17 is not null and :new.D17 != '��' then
    P_EXCEPTION(0, '17 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F17 is null and :new.D17 = '��' then
    P_EXCEPTION(0, '17 �����: ��� �� ��������� ����.');
  end if;

  if :new.F18 is not null and :new.D18 is not null and :new.D18 != '��' then
    P_EXCEPTION(0, '18 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F18 is null and :new.D18 = '��' then
    P_EXCEPTION(0, '18 �����: ��� �� ��������� ����.');
  end if;

  if :new.F19 is not null and :new.D19 is not null and :new.D19 != '��' then
    P_EXCEPTION(0, '19 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F19 is null and :new.D19 = '��' then
    P_EXCEPTION(0, '19 �����: ��� �� ��������� ����.');
  end if;

  if :new.F20 is not null and :new.D20 is not null and :new.D20 != '��' then
    P_EXCEPTION(0, '20 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F20 is null and :new.D20 = '��' then
    P_EXCEPTION(0, '20 �����: ��� �� ��������� ����.');
  end if;

  if :new.F21 is not null and :new.D21 is not null and :new.D21 != '��' then
    P_EXCEPTION(0, '21 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F21 is null and :new.D21 = '��' then
    P_EXCEPTION(0, '21 �����: ��� �� ��������� ����.');
  end if;

  if :new.F22 is not null and :new.D22 is not null and :new.D22 != '��' then
    P_EXCEPTION(0, '22 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F22 is null and :new.D22 = '��' then
    P_EXCEPTION(0, '22 �����: ��� �� ��������� ����.');
  end if;

  if :new.F23 is not null and :new.D23 is not null and :new.D23 != '��' then
    P_EXCEPTION(0, '23 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F23 is null and :new.D23 = '��' then
    P_EXCEPTION(0, '23 �����: ��� �� ��������� ����.');
  end if;

  if :new.F24 is not null and :new.D24 is not null and :new.D24 != '��' then
    P_EXCEPTION(0, '24 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F24 is null and :new.D24 = '��' then
    P_EXCEPTION(0, '24 �����: ��� �� ��������� ����.');
  end if;

  if :new.F25 is not null and :new.D25 is not null and :new.D25 != '��' then
    P_EXCEPTION(0, '25 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F25 is null and :new.D25 = '��' then
    P_EXCEPTION(0, '25 �����: ��� �� ��������� ����.');
  end if;

  if :new.F26 is not null and :new.D26 is not null and :new.D26 != '��' then
    P_EXCEPTION(0, '26 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F26 is null and :new.D26 = '��' then
    P_EXCEPTION(0, '26 �����: ��� �� ��������� ����.');
  end if;

  if :new.F27 is not null and :new.D27 is not null and :new.D27 != '��' then
    P_EXCEPTION(0, '27 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F27 is null and :new.D27 = '��' then
    P_EXCEPTION(0, '27 �����: ��� �� ��������� ����.');
  end if;

  if :new.F28 is not null and :new.D28 is not null and :new.D28 != '��' then
    P_EXCEPTION(0, '28 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F28 is null and :new.D28 = '��' then
    P_EXCEPTION(0, '28 �����: ��� �� ��������� ����.');
  end if;

  if :new.F29 is not null and :new.D29 is not null and :new.D29 != '��' then
    P_EXCEPTION(0, '29 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F29 is null and :new.D29 = '��' then
    P_EXCEPTION(0, '29 �����: ��� �� ��������� ����.');
  end if;

  if :new.F30 is not null and :new.D30 is not null and :new.D30 != '��' then
    P_EXCEPTION(0, '30 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F30 is null and :new.D30 = '��' then
    P_EXCEPTION(0, '30 �����: ��� �� ��������� ����.');
  end if;

  if :new.F31 is not null and :new.D31 is not null and :new.D31 != '��' then
    P_EXCEPTION(0, '31 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F31 is null and :new.D31 = '��' then
    P_EXCEPTION(0, '31 �����: ��� �� ��������� ����.');
  end if;
end;
/
show errors;

create or replace trigger T_PSTSBRD_BUPDATE
  before update on PSTSBRD for each row
begin
  :new.F1 := PKG_EXT.IIF( :new.F1 = 0, null, :new.F1 );
  :new.F2 := PKG_EXT.IIF( :new.F2 = 0, null, :new.F2 );
  :new.F3 := PKG_EXT.IIF( :new.F3 = 0, null, :new.F3 );
  :new.F4 := PKG_EXT.IIF( :new.F4 = 0, null, :new.F4 );
  :new.F5 := PKG_EXT.IIF( :new.F5 = 0, null, :new.F5 );
  :new.F6 := PKG_EXT.IIF( :new.F6 = 0, null, :new.F6 );
  :new.F7 := PKG_EXT.IIF( :new.F7 = 0, null, :new.F7 );
  :new.F8 := PKG_EXT.IIF( :new.F8 = 0, null, :new.F8 );
  :new.F9 := PKG_EXT.IIF( :new.F9 = 0, null, :new.F9 );
  :new.F10 := PKG_EXT.IIF( :new.F10 = 0, null, :new.F10 );
  :new.F11 := PKG_EXT.IIF( :new.F11 = 0, null, :new.F11 );
  :new.F12 := PKG_EXT.IIF( :new.F12 = 0, null, :new.F12 );
  :new.F13 := PKG_EXT.IIF( :new.F13 = 0, null, :new.F13 );
  :new.F14 := PKG_EXT.IIF( :new.F14 = 0, null, :new.F14 );
  :new.F15 := PKG_EXT.IIF( :new.F15 = 0, null, :new.F15 );
  :new.F16 := PKG_EXT.IIF( :new.F16 = 0, null, :new.F16 );
  :new.F17 := PKG_EXT.IIF( :new.F17 = 0, null, :new.F17 );
  :new.F18 := PKG_EXT.IIF( :new.F18 = 0, null, :new.F18 );
  :new.F19 := PKG_EXT.IIF( :new.F19 = 0, null, :new.F19 );
  :new.F20 := PKG_EXT.IIF( :new.F20 = 0, null, :new.F20 );
  :new.F21 := PKG_EXT.IIF( :new.F21 = 0, null, :new.F21 );
  :new.F22 := PKG_EXT.IIF( :new.F22 = 0, null, :new.F22 );
  :new.F23 := PKG_EXT.IIF( :new.F23 = 0, null, :new.F23 );
  :new.F24 := PKG_EXT.IIF( :new.F24 = 0, null, :new.F24 );
  :new.F25 := PKG_EXT.IIF( :new.F25 = 0, null, :new.F25 );
  :new.F26 := PKG_EXT.IIF( :new.F26 = 0, null, :new.F26 );
  :new.F27 := PKG_EXT.IIF( :new.F27 = 0, null, :new.F27 );
  :new.F28 := PKG_EXT.IIF( :new.F28 = 0, null, :new.F28 );
  :new.F29 := PKG_EXT.IIF( :new.F29 = 0, null, :new.F29 );
  :new.F30 := PKG_EXT.IIF( :new.F30 = 0, null, :new.F30 );
  :new.F31 := PKG_EXT.IIF( :new.F31 = 0, null, :new.F31 );

  :new.FD := PKG_EXT.IIF( :new.FD = 0, null, :new.FD );
  :new.FH := PKG_EXT.IIF( :new.FH = 0, null, :new.FH );

  if :new.F1 is not null and :new.D1 is not null and :new.D1 != '��' then
    P_EXCEPTION(0, '1 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F1 is null and :new.D1 = '��' then
    P_EXCEPTION(0, '1 �����: ��� �� ��������� ����.');
  end if;

  if :new.F2 is not null and :new.D2 is not null and :new.D2 != '��' then
    P_EXCEPTION(0, '2 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F2 is null and :new.D2 = '��' then
    P_EXCEPTION(0, '2 �����: ��� �� ��������� ����.');
  end if;

  if :new.F3 is not null and :new.D3 is not null and :new.D3 != '��' then
    P_EXCEPTION(0, '3 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F3 is null and :new.D3 = '��' then
    P_EXCEPTION(0, '3 �����: ��� �� ��������� ����.');
  end if;

  if :new.F4 is not null and :new.D4 is not null and :new.D4 != '��' then
    P_EXCEPTION(0, '4 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F4 is null and :new.D4 = '��' then
    P_EXCEPTION(0, '4 �����: ��� �� ��������� ����.');
  end if;

  if :new.F5 is not null and :new.D5 is not null and :new.D5 != '��' then
    P_EXCEPTION(0, '5 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F5 is null and :new.D5 = '��' then
    P_EXCEPTION(0, '5 �����: ��� �� ��������� ����.');
  end if;

  if :new.F6 is not null and :new.D6 is not null and :new.D6 != '��' then
    P_EXCEPTION(0, '6 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F6 is null and :new.D6 = '��' then
    P_EXCEPTION(0, '6 �����: ��� �� ��������� ����.');
  end if;

  if :new.F7 is not null and :new.D7 is not null and :new.D7 != '��' then
    P_EXCEPTION(0, '7 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F7 is null and :new.D7 = '��' then
    P_EXCEPTION(0, '7 �����: ��� �� ��������� ����.');
  end if;

  if :new.F8 is not null and :new.D8 is not null and :new.D8 != '��' then
    P_EXCEPTION(0, '8 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F8 is null and :new.D8 = '��' then
    P_EXCEPTION(0, '8 �����: ��� �� ��������� ����.');
  end if;

  if :new.F9 is not null and :new.D9 is not null and :new.D9 != '��' then
    P_EXCEPTION(0, '9 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F9 is null and :new.D9 = '��' then
    P_EXCEPTION(0, '9 �����: ��� �� ��������� ����.');
  end if;

  if :new.F10 is not null and :new.D10 is not null and :new.D10 != '��' then
    P_EXCEPTION(0, '10 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F10 is null and :new.D10 = '��' then
    P_EXCEPTION(0, '10 �����: ��� �� ��������� ����.');
  end if;

  if :new.F11 is not null and :new.D11 is not null and :new.D11 != '��' then
    P_EXCEPTION(0, '11 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F11 is null and :new.D11 = '��' then
    P_EXCEPTION(0, '11 �����: ��� �� ��������� ����.');
  end if;

  if :new.F12 is not null and :new.D12 is not null and :new.D12 != '��' then
    P_EXCEPTION(0, '12 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F12 is null and :new.D12 = '��' then
    P_EXCEPTION(0, '12 �����: ��� �� ��������� ����.');
  end if;

  if :new.F13 is not null and :new.D13 is not null and :new.D13 != '��' then
    P_EXCEPTION(0, '13 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F13 is null and :new.D13 = '��' then
    P_EXCEPTION(0, '13 �����: ��� �� ��������� ����.');
  end if;

  if :new.F14 is not null and :new.D14 is not null and :new.D14 != '��' then
    P_EXCEPTION(0, '14 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F14 is null and :new.D14 = '��' then
    P_EXCEPTION(0, '14 �����: ��� �� ��������� ����.');
  end if;

  if :new.F15 is not null and :new.D15 is not null and :new.D15 != '��' then
    P_EXCEPTION(0, '15 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F15 is null and :new.D15 = '��' then
    P_EXCEPTION(0, '15 �����: ��� �� ��������� ����.');
  end if;

  if :new.F16 is not null and :new.D16 is not null and :new.D16 != '��' then
    P_EXCEPTION(0, '16 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F16 is null and :new.D16 = '��' then
    P_EXCEPTION(0, '16 �����: ��� �� ��������� ����.');
  end if;

  if :new.F17 is not null and :new.D17 is not null and :new.D17 != '��' then
    P_EXCEPTION(0, '17 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F17 is null and :new.D17 = '��' then
    P_EXCEPTION(0, '17 �����: ��� �� ��������� ����.');
  end if;

  if :new.F18 is not null and :new.D18 is not null and :new.D18 != '��' then
    P_EXCEPTION(0, '18 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F18 is null and :new.D18 = '��' then
    P_EXCEPTION(0, '18 �����: ��� �� ��������� ����.');
  end if;

  if :new.F19 is not null and :new.D19 is not null and :new.D19 != '��' then
    P_EXCEPTION(0, '19 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F19 is null and :new.D19 = '��' then
    P_EXCEPTION(0, '19 �����: ��� �� ��������� ����.');
  end if;

  if :new.F20 is not null and :new.D20 is not null and :new.D20 != '��' then
    P_EXCEPTION(0, '20 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F20 is null and :new.D20 = '��' then
    P_EXCEPTION(0, '20 �����: ��� �� ��������� ����.');
  end if;

  if :new.F21 is not null and :new.D21 is not null and :new.D21 != '��' then
    P_EXCEPTION(0, '21 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F21 is null and :new.D21 = '��' then
    P_EXCEPTION(0, '21 �����: ��� �� ��������� ����.');
  end if;

  if :new.F22 is not null and :new.D22 is not null and :new.D22 != '��' then
    P_EXCEPTION(0, '22 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F22 is null and :new.D22 = '��' then
    P_EXCEPTION(0, '22 �����: ��� �� ��������� ����.');
  end if;

  if :new.F23 is not null and :new.D23 is not null and :new.D23 != '��' then
    P_EXCEPTION(0, '23 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F23 is null and :new.D23 = '��' then
    P_EXCEPTION(0, '23 �����: ��� �� ��������� ����.');
  end if;

  if :new.F24 is not null and :new.D24 is not null and :new.D24 != '��' then
    P_EXCEPTION(0, '24 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F24 is null and :new.D24 = '��' then
    P_EXCEPTION(0, '24 �����: ��� �� ��������� ����.');
  end if;

  if :new.F25 is not null and :new.D25 is not null and :new.D25 != '��' then
    P_EXCEPTION(0, '25 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F25 is null and :new.D25 = '��' then
    P_EXCEPTION(0, '25 �����: ��� �� ��������� ����.');
  end if;

  if :new.F26 is not null and :new.D26 is not null and :new.D26 != '��' then
    P_EXCEPTION(0, '26 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F26 is null and :new.D26 = '��' then
    P_EXCEPTION(0, '26 �����: ��� �� ��������� ����.');
  end if;

  if :new.F27 is not null and :new.D27 is not null and :new.D27 != '��' then
    P_EXCEPTION(0, '27 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F27 is null and :new.D27 = '��' then
    P_EXCEPTION(0, '27 �����: ��� �� ��������� ����.');
  end if;

  if :new.F28 is not null and :new.D28 is not null and :new.D28 != '��' then
    P_EXCEPTION(0, '28 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F28 is null and :new.D28 = '��' then
    P_EXCEPTION(0, '28 �����: ��� �� ��������� ����.');
  end if;

  if :new.F29 is not null and :new.D29 is not null and :new.D29 != '��' then
    P_EXCEPTION(0, '29 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F29 is null and :new.D29 = '��' then
    P_EXCEPTION(0, '29 �����: ��� �� ��������� ����.');
  end if;

  if :new.F30 is not null and :new.D30 is not null and :new.D30 != '��' then
    P_EXCEPTION(0, '30 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F30 is null and :new.D30 = '��' then
    P_EXCEPTION(0, '30 �����: ��� �� ��������� ����.');
  end if;

  if :new.F31 is not null and :new.D31 is not null and :new.D31 != '��' then
    P_EXCEPTION(0, '31 �����: ���� ��������� ������ ��� ��.');
  end if;
  if :new.F31 is null and :new.D31 = '��' then
    P_EXCEPTION(0, '31 �����: ��� �� ��������� ����.');
  end if;
end;
/
show errors;
