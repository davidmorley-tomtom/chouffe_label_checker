

create.custom.pg.funcs <- function(con) {
  execute.pg(con, 
    "create or replace function normalised_azi_diff(atlas geometry, mds geometry)
    returns float
    language plpgsql immutable as
    	$func$
    	declare
    		a geometry;
    		b geometry;
    		azi float;
    		azin float;
    	begin	
    		if (st_length(atlas) < st_length(mds)) then
    			a := mds;
    			b := atlas;
    		else
    			a := atlas;
    			b := mds;
    		end if;
    	
    		azi := abs(degrees(st_azimuth(
    			 			st_closestpoint(a, st_startpoint(b)), 
    						st_closestpoint(a, st_endpoint(b)))) - 
    							degrees(st_azimuth(st_startpoint(b), st_endpoint(b))));
    										
    		select case 
    			when azi is null then 90
    			when azi > 90 and azi <= 180 then abs(azi - 180)
    			when azi > 180 and azi <= 270 then abs(azi - 270)
    			when azi > 270 then abs(azi - 360)
    			else azi end into azin;
    		return azin;
    	end;
    $func$")
  
  execute.pg(con, 
             "create or replace function common_lines(a geometry, b geometry, d float)
    returns geometry
    language plpgsql immutable as
    	$func$
    	declare
    		line_intr geometry;
    		single_part geometry;
    		best_line geometry;
    		multi_part geometry[];
    		len0 float;
    		len1 float;
    	begin	
    		line_intr := st_intersection(a, st_buffer(b, d * 0.00001, 'endcap=flat join=round'));
    			
    		if (ST_NumGeometries(line_intr) > 1) then
    			select array(select (st_dump(line_intr)).geom) into multi_part;
					len0 := 100000;
    			best_line := multi_part[1];
    			foreach single_part in array multi_part loop
    				len1 := st_hausdorffdistance(single_part, b);
    				if (len1 < len0) then
    					best_line := single_part;
    					len0 := len1;
    				end if;
		    	end loop;
    		else
    			best_line := line_intr;
    		end if;
    	 
    		if (st_isempty(best_line)) then
    			return null;
    		else 
    			return best_line;
    		end if;	
    	end;
    $func$")
}













